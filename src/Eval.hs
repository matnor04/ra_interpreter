module Eval where

import Common
import Control.Applicative(Applicative(..))
import Control.Monad(liftM,ap, filterM)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Function(on)
import Data.List (nub, (\\), intersect, maximumBy, minimumBy, foldl')

-- Mónada con estado (catálogo de tablas) y posibilidad de error 
newtype ReaderM m e a = R {runR :: e -> m a}

instance Monad m => Monad (ReaderM m e) where 
    return x = R (\e -> return x)
    (R f) >>= k = R (\e -> do a <- f e
                              let x = k a in runR x e)

instance Monad m => Functor (ReaderM m e) where 
    fmap = liftM

instance Monad m => Applicative (ReaderM m e) where 
    pure = return 
    (<*>) = ap

-- Mónada para capturar errores
data Result a = Undef NombreRel     -- nombre no encontrado
                | Impos String      -- operación AR no se puede hacer
                | Res a             -- ejecución exitosa
                 

instance Monad Result where 
    return = Res
    (Res x) >>= f = f x
    (Undef t) >>= f = Undef t
    (Impos str) >>= f = Impos str

instance Functor Result where 
    fmap = liftM

instance Applicative Result where 
    pure = return 
    (<*>) = ap

-- Funciones auxiliares para manejar las mónadas
ask :: Monad m => ReaderM m e e 
ask = R (\e -> return e)

undef :: NombreRel -> ReaderM Result Catalogo a 
undef t = R (\_ -> Undef t) -- tabla no está en catálogo

impossible :: String -> ReaderM Result Catalogo a
impossible s = R (\_ -> Impos s) -- la operación no se puede hacer

update :: NombreRel -> Tabla -> ReaderM Result Catalogo Catalogo 
update x t = R (\e -> return (M.insert x t e)) 

liftResult :: Result a -> ReaderM Result e a
liftResult res = R (\_ -> res)

-- evaluación de términos y predicados
evalTerm :: Term -> Fila -> Result Valor
evalTerm (Lit v) _ = return v
evalTerm (Col nom) fila = 
    case M.lookup nom fila of
        Just v -> return v
        Nothing  -> Impos ("Atributo no encontrado: " ++ nom)

evalPred :: Pred -> Fila -> Result Bool
evalPred PTrue _ = return True
evalPred PFalse _ = return False

evalPred (Not p) fila = do b <- evalPred p fila
                           return (not b)

evalPred (And p1 p2) fila = do b1 <- evalPred p1 fila 
                               b2 <- evalPred p2 fila
                               return (b1 && b2)
evalPred (Or p1 p2) fila  = do b1 <- evalPred p1 fila 
                               b2 <- evalPred p2 fila
                               return (b1 || b2)
evalPred (Comp op t1 t2) fila = do
    v1 <- evalTerm t1 fila
    v2 <- evalTerm t2 fila
    case op of 
        Eq -> return (v1 == v2)
        Neq -> return (v1 /= v2)
        Lt -> return (v1 < v2)
        Gt -> return (v1 > v2)
        Le -> return (v1 <= v2)
        Ge -> return (v1 >= v2)

       
-- Auxiliares
-- (obtener esquema)
getEsq :: Tabla -> Esquema
getEsq [] = []
getEsq t = M.keys (head t)

-- (chequear igualdad de esquemas)
compEsq :: Tabla -> Tabla -> Bool
compEsq [] _ = True
compEsq _ [] = True
compEsq t t' = getEsq t == getEsq t' 

-- (extrae lista de enteros asociados a un atributo)
getInts :: Atributo -> [Fila] -> Result [Int]
getInts col filas = mapM checkFila filas
  where
    checkFila fila = case M.lookup col fila of
        Nothing -> Impos ("Columna no encontrada: " ++ col)
        Just (N i) -> return i
        Just (Str s) -> Impos ("Error de Tipo: No se puede sumar/promediar la columna " 
                               ++ show col ++ " porque contiene texto (" ++ show s ++ ")")

-- función de agregación para un grupo de filas, retorna (nombre, valor_calculado)
calcAg :: [Fila] -> (AgFun, Atributo) -> Result (Atributo, Valor)
calcAg filas (f, nom) = case f of
    FCount -> return (nom, N (length filas))
    
    FSum col -> do
        valores <- getInts col filas
        return (nom, N (sum valores))
        
    FAvg col -> do
        valores <- getInts col filas
        if null valores 
           then return (nom, N 0) 
           else return (nom, N (sum valores `div` length valores))

    FMax col -> 
        if null filas then return (nom, N 0)
        else case M.lookup col (head filas) of
            Nothing -> Impos ("El atributo " ++ col ++ " no existe para Max")
            Just _  -> do
                -- M.! es seguro aquí porque ya verificamos con lookup que la col existe
                let val fila = fila M.! col
                -- maximumBy compara filas enteras basándose en el valor de esa columna
                let filaMax = maximumBy (compare `on` val) filas
                return (nom, val filaMax)

    FMin col -> 
        if null filas then return (nom, N 0)
        else case M.lookup col (head filas) of
            Nothing -> Impos ("El atributo " ++ col ++ " no existe para Min")
            Just _  -> do
                let val fila = fila M.! col
                let filaMin = minimumBy (compare `on` val) filas
                return (nom, val filaMin)

-- evaluación de predicados en fila
-- monádico para arrastrar errores de tipo
cumple :: Pred -> Fila -> Result Bool
cumple (And p1 p2) fila =
     do b1 <- cumple p1 fila
        b2 <- cumple p2 fila
        return (b1 && b2)
    
cumple (Or p1 p2) fila = 
    do b1 <- cumple p1 fila
       b2 <- cumple p2 fila
       return (b1 || b2)

cumple (Not p) fila =
    do b <- cumple p fila
       return (not b)

cumple (Comp Eq t1 t2) fila = comparar t1 t2 fila (==) (==) "(=)"
cumple (Comp Neq t1 t2) fila = comparar t1 t2 fila (/=) (/=) "(<>)"
cumple (Comp Gt t1 t2) fila = comparar t1 t2 fila (>) (>) "(>)"
cumple (Comp Lt t1 t2) fila = comparar t1 t2 fila (<) (<) "(<)"
cumple (Comp Ge t1 t2) fila = comparar t1 t2 fila (>=) (>=) "(>=)"
cumple (Comp Le t1 t2) fila = comparar t1 t2 fila (<=) (<=) "(<=)"

-- función auxiliar para detectar errores de tipos en comparaciones
comparar :: Term -> Term -> Fila 
         -> (Int -> Int -> Bool)       -- comparar int
         -> (String -> String -> Bool) -- comparar string
         -> String                     -- mensaje de error
         -> Result Bool
comparar t1 t2 fila opN opStr nomOp =
    do  v1 <- evalTerm t1 fila
        v2 <- evalTerm t2 fila
        case (v1, v2) of
            -- tipos correctos
            (N n1, N n2)     -> return (n1 `opN` n2)
            (Str s1, Str s2) -> return (s1 `opStr` s2)
            -- error de tipo
            (val1, val2)     -> Impos $ "Error de Tipo: No se puede comparar " 
                                    ++ show val1 ++ " con " ++ show val2 
                                    ++ " usando el operador '" ++ nomOp ++ "'"

-- evaluación de expresiones AR
eval :: ARExp -> ReaderM Result Catalogo Tabla 
eval (Tab nom) = do cat <- ask
                    case M.lookup nom cat of  -- busca la tabla en el catálogo
                        Nothing -> undef nom
                        Just t -> return t

eval (Sel p exp) = do tabla <- eval exp
                      liftResult $ filterM (cumple p) tabla
                      
eval (Proy atr exp) = do t <- eval exp 
                         if null t 
                            then return []
                            else let esq = getEsq t
                                     miss = filter (`notElem` esq) atr
                                 in if not (null miss) -- buscamos atributos que no estén en la tabla
                                    then impossible ("Atributos no encontrados: " ++ show miss)                                     
                                    else return (map (\row -> M.restrictKeys row (S.fromList atr)) t)

eval (Dif exp1 exp2) = do t1 <- eval exp1 
                          t2 <- eval exp2
                          if (compEsq t1 t2) 
                            then return (filter (`notElem` t2) t1)
                            else impossible "Esquemas no compatibles para Diferencia"


eval (Union exp1 exp2) = do t1 <- eval exp1 
                            t2 <- eval exp2
                            if (compEsq t1 t2) 
                                then return (nub (t1 ++ t2))
                                else impossible "Esquemas no compatibles para Unión"

eval (Cart exp1 exp2) = do t1 <- eval exp1 
                           t2 <- eval exp2
                           if null t1 || null t2
                              then return []
                              else return [M.union x y | x <- t1, y <- t2]

eval (Renom x exp) = do t <- eval exp
                        -- renombramos todos los atributos del resultado
                        return (map (M.mapKeys (\y -> x ++ "_" ++ y)) t)

eval (Inters exp1 exp2) = eval (Dif exp1 (Dif exp1 exp2))

eval (NatJoin exp1 exp2) = 
    do t1 <- eval exp1 
       t2 <- eval exp2 
       if null t1 || null t2
          then return []
          else do let esq1 = M.keys (head t1)
                  let esq2 = M.keys (head t2)
                  let comm = intersect esq1 esq2
                  return [M.union x y | x <- t1, y <- t2, -- igualdad de atributos comunes
                                        all (\k -> x M.! k == y M.! k) comm]

eval (Div exp1 exp2) = 
    do t1 <- eval exp1 
       t2 <- eval exp2 
       if null t1 || null t2
          then return []
          else do let esq1 = M.keys (head t1)
                  let esq2 = M.keys (head t2)
                  let diff = esq2 \\ esq1 -- diferencia de esquemas
                  if not (null diff) 
                    then impossible ("No se puede hacer división, faltan atributos")
                    else do 
                        let resCols = esq1 \\ esq2
                        let resSet  = S.fromList resCols
                        let candidates = nub $ map (\r -> M.restrictKeys r resSet) t1
                        let isValid candidate = all (\s -> M.union candidate s `elem` t1) t2
                        return (filter isValid candidates)

-- agrupamiento
eval (Agrup colsAgrup opsAg exp) = do
    tabla <- eval exp
    
    if null tabla 
       then return [] -- Si la tabla está vacía, el resultado es vacío
       else do
           -- verificar que las columnas de agrupación existan 
           let colsValidas = M.keys (head tabla)
           if not (all (`elem` colsValidas) colsAgrup)
              then impossible "Columnas de agrupación no encontradas en la tabla"
              else do
                  -- extraer la clave de agrupación de una fila
                  let obtenerClave fila = map (fila M.!) colsAgrup
                  
                  -- insertamos cada fila en su grupo correspondiente 
                  let mapaAgr = foldl' (\ac fila -> M.insertWith (++) (obtenerClave fila) [fila] ac) M.empty tabla
                  
                  --  Definir la lógica de procesamiento por grupo (Pura, dentro de la mónada Result)
                  let procGrupo (clave, filasGrupo) = do
                          -- parte correspondiente a las claves
                          let parteClave = M.fromList (zip colsAgrup clave)
                          -- agregaciones
                          resultadosAg <- mapM (calcAg filasGrupo) opsAg
                          let parteAgregacion = M.fromList resultadosAg
                          
                          -- unir para formar la fila
                          return (M.union parteClave parteAgregacion)

                  -- hacerlo con todos los grupos
                  let grupos = M.toList mapaAgr 
                  let res = map procGrupo grupos 
                  
                  -- elevar
                  case sequence res of
                      Res res' -> return res'
                      Undef n -> undef n
                      Impos s -> impossible s


-- evaluación de sentencias
evalStmt :: Stmt -> Catalogo -> Result (Maybe Tabla, Catalogo)
evalStmt (Query exp) cat =
     case runR (eval exp) cat of -- evaluar la expresión AR
       Res t -> Res (Just t, cat)
       Undef n -> Undef n
       Impos m -> Impos m

evalStmt (Seq s1 s2) cat = do (_, cat') <- evalStmt s1 cat
                              evalStmt s2 cat' -- evaluar con el catálogo resultante

evalStmt (Assign x exp) cat = 
    case M.lookup x cat of -- nos fijamos si el nombre ya está en uso
      Nothing -> case runR (eval exp) cat of 
                   Res t -> return (Nothing, M.insert x t cat)
                   Undef n -> Undef n
                   Impos m -> Impos m
      Just _ -> Impos ("La tabla " ++ x ++ " ya existe")