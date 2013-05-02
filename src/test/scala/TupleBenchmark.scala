
import org.scalameter.api._
import tuplicity._

object TupleBenchmark extends PerformanceTest.Microbenchmark {

	val repetitions = Gen.single("repetitions")(10)

	performance of "Tuplicity Tuple" in {
		measure method "apply(1,2,3)" in {
			using (repetitions) in { r =>
				for(_ <- 1 to r) Tuple(1,2,3)
			}
		}

		measure method "apply(1,2,3,1.2)" in {
			using (repetitions) in { r =>
				for (_ <- 1 to r) Tuple(1,2,3,1.2)
			}
		}

		measure method """apply(1,2,"x",3,1.2,"a")""" in {
			using (repetitions) in { r =>
				for (_ <- 1 to r) Tuple(1,2,"x",3,1.2,"a")
			}
		}

		measure method "apply(1,2,3)._1" in {
			using (repetitions) in { r =>
				for(_ <- 1 to r) Tuple(1,2,3)
			}
		}

		measure method "apply(1,2,3,1.2)._1" in {
			using (repetitions) in { r =>
				for (_ <- 1 to r) Tuple(1,2,3,1.2)._1
			}
		}

		measure method """apply(1,2,"x",3,1.2,"a")._1""" in {
			using (repetitions) in { r =>
				for (_ <- 1 to r) Tuple(1,2,"x",3,1.2,"a")._1
			}
		}

		measure method """apply(1,2,"x",3,1.2,"a")._3""" in {
			using (repetitions) in { r =>
				for (_ <- 1 to r) Tuple(1,2,"x",3,1.2,"a")._3
			}
		}

		measure method """apply(1,2,"x",3,1,2,("a": Any))._3""" in {
			using (repetitions) in { r =>
				for (_ <- 1 to r) Tuple(1,2,"x",3,1.2,("a": Any))._3
			}
		}

	}

	performance of "Native Tuple" in {

		measure method "apply(1,2,3)" in {
			using (repetitions) in { r =>
				for(_ <- 1 to r) (1,2,3)
			}
		}

		measure method "apply(1,2,3,1.2)" in {
			using (repetitions) in { r =>
				for (_ <- 1 to r) (1,2,3,1.2)
			}
		}

		measure method """apply(1,2,"x",3,1.2,"a")""" in {
			using (repetitions) in { r =>
				for (_ <- 1 to r) (1,2,"x",3,1.2,"a")
			}
		}

		measure method "apply(1,2,3)._1" in {
			using (repetitions) in { r =>
				for(_ <- 1 to r) (1,2,3)._1
			}
		}

		measure method "apply(1,2,3,1.2)._1" in {
			using (repetitions) in { r =>
				for (_ <- 1 to r) (1,2,3,1.2)._1
			}
		}

		measure method """apply(1,2,"x",3,1.2,"a")._1""" in {
			using (repetitions) in { r =>
				for (_ <- 1 to r) (1,2,"x",3,1.2,"a")._1
			}
		}

		measure method """apply(1,2,"x",3,1.2,"a")._3""" in {
			using (repetitions) in { r =>
				for (_ <- 1 to r) (1,2,"x",3,1.2,"a")._3
			}
		}

		measure method """apply(1,2,"x",3,1.2,("a": Any))._3""" in {
			using (repetitions) in { r =>
				for (_ <- 1 to r) (1,2,"x",3,1.2,("a": Any))._3
			}
		}

	}

}
