module QueryBuild
( buildDeclaration
, buildImplementation
, intFileName
, impFileName
) where
import CoreDataHs

normalOperators :: [(String, String)]
normalOperators = [
            ("=",  "IsEqualTo"),
            ("<",  "IsLessThan"),
            (">",  "IsGreaterThan"),
            (">=", "IsGreaterThanOrEqualTo"),
            ("<=", "IsLessThanOrEqualTo"),
            ("!=", "IsNotEqualTo"),
            ("BETWEEN", "IsBetwixt")
            ]

stringOperators :: [(String, String)]
stringOperators = [
                ("LIKE", "IsLike"),
                ("CONTAINS", "Contains"),
                ("MATCHES", "Matches"),
                ("BEGINSWITH", "BeginsWith"),
                ("ENDSWITH", "EndsWith")
                ]

fetchRequest :: Entity -> String
fetchRequest e = "\tNSFetchRequest *fetchRequest = [NSFetchRequest fetchRequestWithEntityName:" ++ "@\"" ++ entityName e ++ "\"];\n"

setPredicate :: Attribute -> String -> String
setPredicate a s = "\t[fetchRequest setPredicate:[NSPredicate predicateWithFormat:" ++ "@\"" ++ attrName a ++  " " ++ s ++ " %@\", " ++ "object]];\n"

sortDescriptor :: String
sortDescriptor = "\t[fetchRequest setSortDescriptors:sort];\n"

errorVar :: String
errorVar = "\tNSError *err = nil;\n"

executeFetch :: String
executeFetch = "\tNSArray *results = [context executeFetchRequest:fetchRequest error:&err];\n"

errorBlock :: String
errorBlock = "\tif(err && errorBlock) {\n" ++ "\t\terrorBlock(err);\n" ++ "\t\treturn nil;\n" ++ "\t}\n"

signature :: String
signature = ":(id)object " ++ "inContext:(NSManagedObjectContext *)context " ++ "sortDescriptors:(NSArray *)sort" ++  " error:(void(^)(NSError *error))errorBlock"

intFileName :: Entity -> String
intFileName e = entityName e ++ "_Fetcher.h"

impFileName :: Entity -> String
impFileName e = entityName e ++ "_Fetcher.m"

buildDeclaration :: Entity -> String
buildDeclaration e = final
                      where
                      imports = "#import <CoreData/CoreData.h>\n"
                                   ++ "#import <Foundation/Foundation.h>\n"
                      declaration = "@interface " ++ entityName e ++ " (Fetcher)\n\n"

                      requests = map buildQueryDeclaration (entityAttributes e)
                      stringDec = map buildQueryStringDeclaration (entityAttributes e)
                      end = "@end\n\n"
                      final = imports ++ declaration ++ concat requests ++ concat stringDec ++ end


buildImplementation :: Entity -> String
buildImplementation e = final
                      where
                      declaration = "@implementation " ++ entityName e ++ " (Fetcher)\n"

                      requests = [buildQueryImplementation x y | x <- entityAttributes e, y <- [e]]
                      stringReq = [buildQueryStringImplementation x y | x <- entityAttributes e, y <- [e]]
                      end = "@end\n\n"
                      final = declaration ++ concat requests ++ concat stringReq ++ end

buildQueryDeclaration :: Attribute -> String
buildQueryDeclaration a = concat ["+ (NSArray *)" ++ y ++ z ++ signature ++ ";\n\n" | (_, z) <- normalOperators, y <- [attrName a]]

buildQueryStringDeclaration :: Attribute -> String
buildQueryStringDeclaration a = concat ["+ (NSArray *)" ++ attrName y ++ z ++ signature ++ ";\n\n" | (_, z) <- stringOperators, y <- [a], attrType a == "String"]

buildQueryImplementation :: Attribute -> Entity -> String
buildQueryImplementation a e = concat ["+ (NSArray *)" ++ y ++ z ++ signature ++ " {\n" ++ fetchRequest e  ++ setPredicate a x ++ sortDescriptor ++ errorVar ++ executeFetch ++ errorBlock ++ "\treturn results;" ++ "\n}\n\n" | (x, z) <- normalOperators, y <- [attrName a]]

buildQueryStringImplementation :: Attribute -> Entity -> String
buildQueryStringImplementation a e = concat ["+ (NSArray *)" ++ attrName y ++ z ++ signature ++ " {\n" ++ fetchRequest e  ++ setPredicate a x ++ sortDescriptor ++ errorVar ++ executeFetch ++ errorBlock ++ "\treturn results;" ++ "\n}\n\n" | (x, z) <- stringOperators, y <- [a], attrType a == "String"]
