package focusedreading.imitation_learning
import com.redis.RedisClient
import focusedreading.reinforcement_learning.actions.{ExploreEndpoints_ExploitQuery, FocusedReadingAction}
import focusedreading.reinforcement_learning.states.FocusedReadingState
import focusedreading.supervision.search.executable.DoSearch

class RedisCache(server:String = "localhost", port:Int = 6379) extends SolutionsCache {

  val redisClient = new RedisClient(server, port)

  override def apply(state: FocusedReadingState): Seq[DoSearch.Result] = {
    val key = state.hashCode()
    redisClient.reconnect
    val elementsIds =  redisClient.lrange(s"optimalSequence:$key", 0, -1)
    val elements = elementsIds match {
      case Some(ids) =>
        ids collect {
          case Some(id) =>
            redisClient.hgetall1[String, String](s"solutionElement:$id") match {
              case Some(m) =>
                DoSearch.Result(
                  FocusedReadingState.fromJson(m("state")),
                  FocusedReadingAction(m("action")),
                  m("cost").toDouble,
                  m("estimatedRemaining").toInt,
                  m("actualRemaining").toInt
                )
              case None =>
                throw new Exception("Shouldn't fall in here. Check!!")
            }
        }
      case None => Seq()
    }
    redisClient.disconnect
    elements
  }

  override def contains(state: FocusedReadingState): Boolean = {
    val key = state.hashCode()
    redisClient.reconnect
    val ret = redisClient.exists(s"optimalSequence:$key")
    redisClient.disconnect
    ret
  }

  override def get(state: FocusedReadingState): Option[Seq[DoSearch.Result]] = contains(state) match {
    case true => Some(this(state))
    case false => None
  }

  override def cache(state: FocusedReadingState, value: Seq[DoSearch.Result]): Unit = {
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

    // All of this in a transaction
    redisClient.reconnect
    // Add the entries as hashes to redis
    redisHashes foreach { case(id, map) => redisClient.hmset(s"solutionElement:$id", map) }
    // Add a list of the ids of the entries as a list into redis and associate it to the key
    val sequenceIds = redisHashes map (_._1)
    val redisKey = s"optimalSequence:$key"
    redisClient.rpush(redisKey, sequenceIds.head, sequenceIds.tail:_*)
    redisClient.disconnect
  }

}
