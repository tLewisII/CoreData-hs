{-# LANGUAGE OverloadedStrings #-}
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
import Data.Text(Text, pack)
import Data.Monoid

data Attribute = Attribute {
                              attrName :: Text
                            , attrType :: Text
                            , entName  :: Text
                           } deriving (Show)
data Entity = Entity {
                        entityName :: Text
                      , entityAttributes :: [Attribute]
                      , entityRelationships :: [Text]
                     } deriving (Show)
                     
simpleName :: String -> QName
simpleName s = QName s Nothing Nothing

typeAttr :: Element -> Text
typeAttr e = case (findAttr $ simpleName "attributeType") e of
				Just a -> pack a
				Nothing -> error "element 'attributeType' not found"

nameAttr :: Element -> Text
nameAttr e = case (findAttr $ simpleName "name") e of
				Just a -> pack a
				Nothing -> error "element 'name' not found"

attrElements :: Element -> [Element]
attrElements = findChildren $ simpleName "attribute"

relChild :: Element -> [Element]
relChild = findChildren $ simpleName "relationship"

relationships :: Element -> [Text]
relationships e = map nameAttr (relChild e)

buildEntity :: Element -> Entity
buildEntity e = Entity (nameAttr e) [buildAttribute x e | x <- attrElements e]  (relationships e)

entityAttrs :: Element -> [Text]
entityAttrs e = map nameAttr (attrElements e)

findEntity :: String -> [Entity] -> Maybe Entity
findEntity "" _ = Nothing
findEntity _ [] = Nothing
findEntity s e = find (\(Entity name _ _) -> name == pack s) e

buildAttribute :: Element -> Element -> Attribute
buildAttribute e b = Attribute (nameAttr e) (typeAttr e) (nameAttr b)

fullModelPath :: String -> String
fullModelPath s = s <> ".xcdatamodeld/" <> s <> ".xcdatamodel/contents"

versionModelPath :: String -> String -> String
versionModelPath s v = s <> ".xcdatamodeld/" <> s <> "_" <> v <> ".xcdatamodel/contents"

modelEntities :: Text -> [Entity]
modelEntities = map buildEntity . concatMap (findElements $ simpleName "entity") . onlyElems . parseXML
