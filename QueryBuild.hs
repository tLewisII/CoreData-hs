{-# LANGUAGE OverloadedStrings #-}
module QueryBuild
( buildDeclaration
, buildImplementation
, intFileName
, impFileName
) where
import CoreDataHs
import Data.Text(Text, pack, concat)
import Data.Monoid
import Prelude hiding(concat)

normalOperators :: [(Text, Text)]
normalOperators = [
            ("=",  "IsEqualTo"),
            ("<",  "IsLessThan"),
            (">",  "IsGreaterThan"),
            (">=", "IsGreaterThanOrEqualTo"),
            ("<=", "IsLessThanOrEqualTo"),
            ("!=", "IsNotEqualTo"),
            ("BETWEEN", "IsBetwixt")
            ]

stringOperators :: [(Text, Text)]
stringOperators = [
                ("LIKE", "IsLike"),
                ("CONTAINS", "Contains"),
                ("MATCHES", "Matches"),
                ("BEGINSWITH", "BeginsWith"),
                ("ENDSWITH", "EndsWith")
                ]

fetchRequest :: Entity -> Text
fetchRequest (Entity name _ _) = "\tNSFetchRequest *fetchRequest = [NSFetchRequest fetchRequestWithEntityName:" 
                                  <> "@\"" 
                                  <> name 
                                  <> "\"];\n"

setPredicate :: Attribute -> Text -> Text
setPredicate a s = "\t[fetchRequest setPredicate:[NSPredicate predicateWithFormat:" 
                    <> "@\"" 
                    <> attrName a 
                    <>  " " 
                    <> s 
                    <> " %@\", " 
                    <> "object]];\n"

sortDescriptor :: Text
sortDescriptor = "\t[fetchRequest setSortDescriptors:sort];\n"

errorVar :: Text
errorVar = "\tNSError *err = nil;\n"

executeFetch :: Text
executeFetch = "\tNSArray *results = [context executeFetchRequest:fetchRequest error:&err];\n"

errorBlock :: Text
errorBlock = "\tif(!results && errorBlock) {\n" 
              <> "\t\terrorBlock(err);\n" 
              <> "\t\treturn nil;\n" 
              <> "\t}\n"

signature :: Text
signature = ":(id)object " 
            <> "inContext:(NSManagedObjectContext *)context " 
            <> "sortDescriptors:(NSArray *)sort" 
            <>  " error:(void(^)(NSError *error))errorBlock"

intFileName :: Entity -> Text
intFileName (Entity name _ _) = name <> "_Fetcher.h"

impFileName :: Entity -> Text
impFileName (Entity name _ _) = name <> "_Fetcher.m"

buildDeclaration :: Entity -> Text
buildDeclaration (Entity name attributes _) = final
                      where
                      imports = "#import <CoreData/CoreData.h>\n"
                                   <> "#import <Foundation/Foundation.h>\n"
                                   <> "#import \"" <> name <> ".h\"" <> "\n\n"
                      declaration = "@interface " <> name <> " (Fetcher)\n\n"
                      requests = map buildQueryDeclaration attributes
                      stringDec = map buildQueryStringDeclaration attributes
                      end = "@end\n\n"
                      final = imports <> declaration <> concat requests <> concat stringDec <> end


buildImplementation :: Entity -> Text
buildImplementation e@(Entity name attributes _) = final
                      where
                      imports = "#import \"" <> name <> "_Fetcher.h\"" <> "\n\n"                      
                      declaration = "@implementation " <> name <> " (Fetcher)\n"
                      requests = [buildQueryImplementation x e | x <- attributes]
                      stringReq = [buildQueryStringImplementation x e | x <- attributes]
                      end = "@end\n\n"
                      final = imports <> declaration <> concat requests <> concat stringReq <> end

buildQueryDeclaration :: Attribute -> Text
buildQueryDeclaration (Attribute name _ _) = concat ["+ (NSArray *)" 
                                                      <> name 
                                                      <> z 
                                                      <> signature 
                                                      <> ";\n\n" 
                                                      | (_, z) <- normalOperators]

buildQueryStringDeclaration :: Attribute -> Text
buildQueryStringDeclaration (Attribute name t _) = concat ["+ (NSArray *)" 
                                                              <> name 
                                                              <> z 
                                                              <> signature 
                                                              <> ";\n\n" 
                                                              | (_, z) <- stringOperators, t == "String"]

buildQueryImplementation :: Attribute -> Entity -> Text
buildQueryImplementation a@(Attribute name _ _) e = concat ["+ (NSArray *)" 
                                                            <> name 
                                                            <> z 
                                                            <> signature 
                                                            <> " {\n" 
                                                            <> fetchRequest e  
                                                            <> setPredicate a x 
                                                            <> sortDescriptor 
                                                            <> errorVar 
                                                            <> executeFetch 
                                                            <> errorBlock 
                                                            <> "\treturn results;" 
                                                            <> "\n}\n\n" 
                                                            | (x, z) <- normalOperators]

buildQueryStringImplementation :: Attribute -> Entity -> Text
buildQueryStringImplementation a@(Attribute name t _) e = concat ["+ (NSArray *)" 
                                                                  <> name 
                                                                  <> z 
                                                                  <> signature 
                                                                  <> " {\n" 
                                                                  <> fetchRequest e 
                                                                  <> setPredicate a x 
                                                                  <> sortDescriptor 
                                                                  <> errorVar 
                                                                  <> executeFetch 
                                                                  <> errorBlock 
                                                                  <> "\treturn results;" 
                                                                  <> "\n}\n\n" 
                                                                  | (x, z) <- stringOperators, t == "String"]
