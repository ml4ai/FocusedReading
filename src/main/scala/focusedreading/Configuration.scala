package focusedreading

import java.{lang, util}

import collection.JavaConversions._
import com.typesafe.config.{Config, ConfigFactory}

object Configuration {

  private[Configuration] abstract class NormalizationConfig (b:Config) {
    private val normConfig = b.getConfig("normalization")

    val enabled: Boolean = normConfig.getBoolean("enabled")
    val lower: Double = normConfig.getDouble("lower")
    val upper: Double = normConfig.getDouble("upper")
    val rangesPath: String = normConfig.getString("rangesFile")

  }

  private  val config = ConfigFactory.load()

  object MDP {
    private  val mdpConf = config.getConfig("MDP")

   val useRewardShaping: Boolean = mdpConf.getBoolean("rewardShaping")
   val rewardShapingCoefficient: Double = mdpConf.getDouble("rewardShapingCoefficient")

    object PaperAmounts {
      private  val paperAmounts = mdpConf.getConfig("paperAmounts")
       val few: Int = paperAmounts.getInt("few")
       val many: Int = paperAmounts.getInt("many")
    }

     val activeActions:Seq[String] = mdpConf.getConfig("actions").getStringList("active")

     val maxIterations: Int = mdpConf.getInt("maxIterations")
     val maxUnchangedIterations: Int = mdpConf.getInt("maxUnchangedIterations")


  }

  object Lucene {
    private  val luceneConf = config.getConfig("lucene")
     val indexPath: String = luceneConf.getString("annotationsIndex")
  }

  object SQLite {
     val dbPath: String = config.getConfig("informationExtraction").getString("sqlitePath")
  }

  object Training {
    private val trainingConf = config.getConfig("training")

    val inputPath: String = trainingConf.getString("inputFile")
    val policyPath: String = trainingConf.getString("policyFile")
    val epochs: Int = trainingConf.getInt("epochs")
    val maxEpisodes: Int = trainingConf.getInt("maxEpisodes")
    val burnInEpisodes: Int = trainingConf.getInt("burnInEpisodes")
    val learningRate: Double = trainingConf.getDouble("learningRate")
    val decayParameter: Double = trainingConf.getDouble("decayParameter")

    object Epsilon{
      private val epConf = trainingConf.getConfig("epsilon")
      val initial: Double = epConf.getDouble("initial")
      val lowerBound: Double = epConf.getDouble("lowerBound")
    }

    object Normalization extends NormalizationConfig(trainingConf)


    val outputRangesFile: String = trainingConf.getString("rangesFile")

  }

  object Imitation {
    private val imitationConf = config.getConfig("imitation")

    val inputPath: String = imitationConf.getString("inputFile")

    val policyPath: String = imitationConf.getString("policyFile")
    val epochs: Int = imitationConf.getInt("epochs")
    val maxEpisodes: Int = imitationConf.getInt("maxEpisodes")

    val initialLearningRate: Double = imitationConf.getDouble("initialLearningRate")

    val activeFeatures:Seq[String] = imitationConf.getStringList("includedFeatures")


    object Normalization extends NormalizationConfig(imitationConf)

    val rangesOutputPath: String = imitationConf.getString("rangesFile")

  }

  object Baseline {
    val inputPath: String = config.getConfig("baseline").getString("inputFile")
  }

  object Testing {
    private val testingConfig = config.getConfig("testing")

    val inputPath: String = testingConfig.getString("inputFile")
    val policyPath: String = testingConfig.getString("policyFile")

    object Normalization extends NormalizationConfig(testingConfig)

    object Output {
      private val outputConfig = testingConfig.getConfig("output")
      val evidencePath: String = outputConfig.getString("evidence")
      val annotationsPath: String = outputConfig.getString("annotations")
      val bootstrapPath: String = outputConfig.getString("bootstrap")
    }
  }

  object ExpertOracle {
    private val oracleConfig = config.getConfig("expertOracle")

    val inputPath: String = oracleConfig.getString("inputFile")
    val goldenDataPath: String = oracleConfig.getString("goldenDataPath")
    val negativeDataPath: String = oracleConfig.getString("negativeDataPath")

    val testingSolutionsPath: String = oracleConfig.getString("solutionsPath")
    val trainingSolutionsPath: String = oracleConfig.getString("trainingSolutionsPath")
    val classifierPath: String = oracleConfig.getString("classifierPath")
    val activeFeatures:Seq[String] = oracleConfig.getStringList("includedFeatures")
    // TODO: Eliminate this
    val classWeights: util.List[lang.Double] = oracleConfig.getDoubleList("classWeights")
  }
}
