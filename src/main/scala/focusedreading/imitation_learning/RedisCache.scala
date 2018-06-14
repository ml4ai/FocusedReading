package focusedreading.imitation_learning
import com.redis.RedisClient
import focusedreading.reinforcement_learning.actions.FocusedReadingAction
import focusedreading.reinforcement_learning.states.FocusedReadingState
import focusedreading.supervision.search.SearchResult

class RedisCache(server:String = "localhost", port:Int = 6379) extends SolutionsCache {

  private implicit val redisClient: RedisClient = new RedisClient(server, port)

  private def withRedis[A](func:(RedisClient)=> A)(implicit client: RedisClient):A = {
    client.reconnect
    val ret = func(client)
    client.disconnect
    ret
  }


  override def apply(state: FocusedReadingState): Seq[SearchResult] = withRedis {
    client =>
      val key = state.hashCode()
      val elementsIds = client.lrange(s"optimalSequence:$key", 0, -1)
      val elements = elementsIds match {
        case Some(ids) =>
          ids collect {
            case Some(id) =>
              client.hgetall1[String, String](s"solutionElement:$id") match {
                case Some(m) =>
                  SearchResult(
                    FocusedReadingState.fromJson(m("state")),
                    FocusedReadingAction(m("action")),
                    m("cost").toDouble,
                    Some(m("estimatedRemaining").toInt),
                    Some(m("actualRemaining").toInt)
                  )
                case None =>
                  throw new Exception("Shouldn't fall in here. Check!!")
              }
          }
        case None => Seq()
      }

      elements
  }


  override def contains(state: FocusedReadingState): Boolean = withRedis { _.exists(s"optimalSequence:${state.hashCode()}") }

  override def get(state: FocusedReadingState): Option[Seq[SearchResult]] = if (contains(state)) {
    Some(this (state))
  } else {
    None
  }

  override def cache(state: FocusedReadingState, value: Seq[SearchResult]): Unit = withRedis {
    client =>
      val key = state.hashCode()
      // Map the value to a sequence of tuples of (hash as id,redis hash string)
      val redisHashes = value map {
        r =>
          //val str = s"state ${r.state.toJson} action ${r.action} cost ${r.cost} estimatedRemaining ${r.estimatedRemaining} actualRemaining ${r.actualRemaining}"
          val m = Map(
            "state" -> r.state.toJson,
            "action" -> r.action,
            "cost" -> r.cost,
            "estimatedRemaining" -> r.estimatedRemaining,
            "actualRemaining" -> r.actualRemaining
          )

          val id = m.hashCode()
          (id, m)
      }

      // Add the entries as hashes to redis
      redisHashes foreach { case(id, map) => client.hmset(s"solutionElement:$id", map) }
      // Add a list of the ids of the entries as a list into redis and associate it to the key
      val sequenceIds = redisHashes map (_._1)
      val redisKey = s"optimalSequence:$key"
      client.rpush(redisKey, sequenceIds.head, sequenceIds.tail:_*)
  }

}
