-- | Main entry point to the application.
module Main where
import Text.XML.Light
import Data.Maybe
import Data.List
-- | The main entry point.
data Attribute = Attribute {
                              attrName :: String
                            , attrType :: String
                           } deriving (Show)
data Entity = Entity {
                        entityName :: String
                      , entityAttributes :: [Attribute]
                      , entityRelationships :: [String]
                     } deriving (Show)

simpleName s = QName s Nothing Nothing

typeAttr :: Element -> String
typeAttr e = fromJust $ (findAttr $ simpleName "attributeType") e

nameAttr :: Element -> String
nameAttr e = fromJust $ (findAttr $ simpleName "name") e

attrElements :: Element -> [Element]
attrElements e = (findChildren $ simpleName "attribute") e

relChild :: Element -> [Element]
relChild e = (findChildren $ simpleName "relationship") e

relationships :: Element -> [String]
relationships e = (map nameAttr (relChild e))

buildEntity :: Element -> Entity
buildEntity e = (Entity (nameAttr e) (map buildAttribute (attrElements e))  (relationships e))

entityAttrs :: Element -> [String]
entityAttrs e = (map nameAttr (attrElements e))

findEntity :: String -> [Entity] -> (Maybe Entity)
findEntity "" _ = Nothing
findEntity _ [] = Nothing
findEntity s e = find (\(Entity name _ _) -> name == s) e

buildAttribute :: Element -> Attribute
buildAttribute e = (Attribute (nameAttr e) (typeAttr e))

main :: IO ()
main = do
  xml <- readFile "TLKCustomerToolKit.xcdatamodeld/TLKCustomerToolKit.xcdatamodel/contents"
  let content     = parseXML xml
      allEntities = concatMap (findElements $ simpleName "entity") (onlyElems content)
      entities    = map buildEntity allEntities
  print entities
