import Numeric.Units.Dimensional.Prelude
import qualified Prelude

main = do
    print $ 1 *~ kilo meter
    print $ 1 *~ (kilo meter / hour)
    print $ 1 *~ newton
    print $ 1 *~ pascal    