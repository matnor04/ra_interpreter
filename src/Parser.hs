module Parser where

import              Text.ParserCombinators.Parsec
import qualified    Text.Parsec.Token as T 
import              Text.Parsec.Language           ( emptyDef )
import              Text.Parsec.Expr 
import              Common

-- TOKENS
ar :: T.TokenParser u 
ar = T.makeTokenParser 
    emptyDef{
        T.reservedNames =
           ["PROJECT", "SELECT", "RENAME", "GROUP",  -- operaciones AR
            "AND","OR","NOT",                        -- operaciones lógicas
            "SUM", "COUNT", "AVG", "MAX", "MIN",     -- funciones de agrupamiento
            "AS",                                    -- reservada para agrupamiento
            "exit", "getcat", "help"],               -- reservada para comandos CLI   
 
        T.reservedOpNames = [
                          "=", "<>", ">", "<","<=",">=", -- comparación
                          "X",   -- producto cartesiano
                          "-",   -- diferencia
                          "|X|", -- producto natural
                          "/",   -- división
                          "U",   -- unión
                          "$",  -- intersección
                          "<-"   -- asignación
                          ,";"
                          ]
    }

whiteSpace = T.whiteSpace ar
integer    = T.integer ar
stringLit  = T.stringLiteral ar
parens     = T.parens ar
brackets   = T.brackets ar
commaSep   = T.commaSep ar
identifier = T.identifier ar
reserved   = T.reserved ar
reservedOp = T.reservedOp ar

totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace 
  t <- p
  eof
  return t

-- función para ayudar con la precedencia
binary name fun assoc = Infix (reservedOp name >> return fun) assoc

-- parseo de predicados de selección
-- precedencias y asociatividad
predTabla = [ [Prefix (reserved "NOT" >> return Not)]
            , [binary "AND" And AssocLeft]
            , [binary "OR"  Or  AssocLeft]
            ]

parsePred :: Parser Pred
parsePred = buildExpressionParser predTabla parseP 

parseP :: Parser Pred
parseP = parens parsePred 
        <|> (reserved "True"  >> return PTrue)
        <|> (reserved "False" >> return PFalse)
        <|> parseComp

parseComp :: Parser Pred
parseComp = do t1 <- parseT 
               op <- parseOp 
               t2 <- parseT
               return (Comp op t1 t2)

parseT :: Parser Term
parseT = (Lit <$> parseVal) <|> (Col <$> identifier)

parseVal :: Parser Valor 
parseVal = (N . fromInteger <$> integer)
         <|> (Str <$> stringLit)
         
parseOp :: Parser Op 
parseOp = (reservedOp "="  >> return Eq)
      <|> (reservedOp "<>" >> return Neq)
      <|> (reservedOp ">=" >> return Ge)
      <|> (reservedOp "<=" >> return Le)
      <|> (reservedOp ">"  >> return Gt)
      <|> (reservedOp "<"  >> return Lt)


-- PARSEO DE EXPRESIONES DE ÁLGEBRA RELACIONAL 

-- asociatividad y precedencia de operadores binarios
tablaOp = [ [binary "X" Cart AssocLeft
          ,binary "|X|" NatJoin AssocLeft
          ],

         [ binary "$" Inters AssocLeft,
           binary "U" Union AssocLeft]

          ,[binary "-" Dif AssocLeft,binary "/" Div AssocLeft]
        ]

parseARExp :: Parser ARExp 
parseARExp = buildExpressionParser tablaOp term

-- unidad básica (tabla, paréntesis u operación unaria)
term :: Parser ARExp
term = parens parseARExp <|> parseUnaria <|> (Tab <$> identifier)

-- parseo de operaciones unarias
parseUnaria :: Parser ARExp
parseUnaria = try parseSelect <|> 
              try parseProject <|> 
              try parseRenom <|>
              try parseAgrup

parseSelect :: Parser ARExp 
parseSelect = do reserved "SELECT"
                 pred <- parens parsePred
                 t <- term
                 return (Sel pred t)

parseProject :: Parser ARExp 
parseProject = do reserved "PROJECT"
                  atr <- brackets (commaSep identifier)
                  t <- term 
                  return (Proy atr t)

parseRenom :: Parser ARExp
parseRenom = do reserved "RENAME"
                x <- identifier
                t <- term 
                return (Renom x t)


-- parsing de agrupamiento

parseFAg :: Parser AgFun -- función de agrupamiento y atributo (si tiene)
parseFAg = choice 
    [ reserved "COUNT" >> return FCount
    , do reserved "SUM"; col <- identifier; return (FSum col)
    , do reserved "AVG"; col <- identifier; return (FAvg col)
    , do reserved "MAX"; col <- identifier; return (FMax col)
    , do reserved "MIN"; col <- identifier; return (FMin col)
    ]

parseAgDef :: Parser (AgFun, Atributo)
parseAgDef = do
    f <- parseFAg
    reserved "AS"
    nom <- identifier -- nombre que le damos a la columna nueva (ej: Total_Ganancias)
    return (f, nom)

parseAgrup :: Parser ARExp
parseAgrup = do reserved "GROUP"
                grupos <- brackets (commaSep identifier)
                aggs <- parens (commaSep parseAgDef)
                exp <- parseARExp
                return (Agrup grupos aggs exp)

-- PARSEO DE SENTENCIAS (consultas o asignaciones)

-- parsea secuencias de sentencias
parseStmt :: Parser Stmt
parseStmt = do stmts <- sepBy1 parseStmt' (reservedOp ";")
               return $ foldl1 Seq stmts
 
parseStmt' :: Parser Stmt
parseStmt' = try parseAssign <|> (Query <$> parseARExp)

parseAssign :: Parser Stmt
parseAssign = do
    nom <- identifier
    reservedOp "<-"
    exp <- parseARExp
    return (Assign nom exp)



