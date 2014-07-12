module CoreDataHs
( modelEntities
, fullModelPath
, versionModelPath
, findEntity
, Entity(Entity)
, Attribute(Attribute, entName, attrType)
, entityName
, entityAttributes
, attrName
) where

import Text.XML.Light
import Data.List

data Attribute = Attribute {
                              attrName :: String
                            , attrType :: String
                            , entName  :: String
                           } deriving (Show)
data Entity = Entity {
                        entityName :: String
                      , entityAttributes :: [Attribute]
                      , entityRelationships :: [String]
                     } deriving (Show)
                     
simpleName :: String -> QName
simpleName s = QName s Nothing Nothing

typeAttr :: Element -> String
typeAttr e = case (findAttr $ simpleName "attributeType") e of
				Just a -> a
				Nothing -> error "element 'attributeType' not found"

nameAttr :: Element -> String
nameAttr e = case (findAttr $ simpleName "name") e of
				Just a -> a
				Nothing -> error "element 'name' not found"

attrElements :: Element -> [Element]
attrElements = findChildren $ simpleName "attribute"

relChild :: Element -> [Element]
relChild = findChildren $ simpleName "relationship"

relationships :: Element -> [String]
relationships e = map nameAttr (relChild e)

buildEntity :: Element -> Entity
buildEntity e = Entity (nameAttr e) [buildAttribute x e | x <- attrElements e]  (relationships e)

entityAttrs :: Element -> [String]
entityAttrs e = map nameAttr (attrElements e)

findEntity :: String -> [Entity] -> Maybe Entity
findEntity "" _ = Nothing
findEntity _ [] = Nothing
findEntity s e = find (\(Entity name _ _) -> name == s) e

buildAttribute :: Element -> Element -> Attribute
buildAttribute e b = Attribute (nameAttr e) (typeAttr e) (nameAttr b)

fullModelPath :: String -> String
fullModelPath s = s ++ ".xcdatamodeld/" ++ s ++ ".xcdatamodel/contents"

versionModelPath :: String -> String -> String
versionModelPath s v = s ++ ".xcdatamodeld/" ++ s ++ "_" ++ v ++ ".xcdatamodel/contents"

modelEntities :: String -> [Entity]
modelEntities = map buildEntity . concatMap (findElements $ simpleName "entity") . onlyElems . parseXML
