module Common where
  
import qualified Data.Map as M    
import Data.Aeson (ToJSON(..), Value(..))
import qualified Data.Text as T  

type Atributo = String                -- nombre de atributo
type NombreRel = String               -- nombre de relación

-- Términos y valores 
-- Se excluyen atributos multivaluados
data Term = Col Atributo
          | Lit Valor
          deriving (Show, Eq)

data Valor = Str String 
            | N Int
            deriving (Show, Eq, Ord)


-- predicados de la selección
data Op = Eq | Neq | Gt | Lt | Ge | Le
         deriving (Show, Eq)

data Pred = PTrue 
          | PFalse
          | And Pred Pred
          | Or Pred Pred
          | Not Pred
          | Comp Op Term Term 
          deriving (Show, Eq)

-- tipos de tablas, filas, estado
type Fila = M.Map Atributo Valor
type Tabla = [Fila]
type Catalogo = M.Map NombreRel Tabla   

type Esquema = [Atributo]

-- funciones de agrupamiento
data AgFun = FCount                   -- contar ocurrencias
           | FSum Atributo            -- sumar
           | FAvg Atributo            -- promedio
           | FMax Atributo            -- máximo
           | FMin Atributo            -- mínimo
           deriving (Show, Eq)

-- Expresiones AR, las fundamentales
data ARExp =  Tab NombreRel
            | Proy [Atributo] ARExp
            | Sel Pred ARExp 
            | Union ARExp ARExp
            | Cart ARExp ARExp
            | Dif ARExp ARExp
            | Renom NombreRel ARExp
            | NatJoin ARExp ARExp 
            | Inters ARExp ARExp 
            | Div ARExp ARExp
            | Agrup [Atributo] [(AgFun, Atributo)] ARExp
            deriving (Show, Eq)

-- Statements
data Stmt =   Query ARExp 
            | Assign NombreRel ARExp 
            | Seq Stmt Stmt
                deriving (Show, Eq)

-- web app
instance ToJSON Valor where
    toJSON (N i) = Number (fromIntegral i)
    toJSON (Str s) = String (T.pack s)
