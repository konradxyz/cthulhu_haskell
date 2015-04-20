module InstanceGatherer where


import qualified Template as Tmpl
import Control.Monad.State
import qualified Ast as Ast

type Gatherer a = StateT (M




instances :: Tmpl.Templates -> [Tmpl.FunctionInstance]
instances
