module QueryBuild
( buildDeclaration
, buildImplementation
, intFileName
, impFileName
) where
import CoreDataHs
operators :: [(String, String)]
operators = [
            ("=",  "IsEqualTo"),
            ("<",  "IsLessThan"),
            (">",  "IsGreaterThan"),
            (">=", "IsGreaterThanOrEqualTo"),
            ("<=", "IsLessThanOrEqualTo"),
            ("!=", "IsNotEqualTo")
            ]

fetchRequest :: Entity -> String
fetchRequest e = "\tNSFetchRequest *fetchRequest = [NSFetchRequest fetchRequestWithEntityName:" ++ "@\"" ++ (entityName e) ++ "\"];\n"

setPredicate :: Attribute -> String -> String
setPredicate a s = "\t[fetchRequest setPredicate:[NSPredicate predicateWithFormat:" ++ "@\"" ++ (attrName a) ++  " " ++ s ++ " %@\", " ++ "object]];\n"

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
intFileName e = (entityName e) ++ "_Fetcher.h"

impFileName :: Entity -> String
impFileName e = (entityName e) ++ "_Fetcher.m"

buildDeclaration :: Entity -> String
buildDeclaration e = final
                      where
                      imports = "#import <CoreData/CoreData.h>\n"
                                   ++ "#import <Foundation/Foundation.h>\n"
                      declaration = "@interface " ++ (entityName e) ++ " (fetch)\n\n"

                      requests = map buildQueryDeclaration (entityAttributes e)
                      end = "@end\n\n"
                      final = imports ++ declaration ++ (concat requests) ++ end


buildImplementation :: Entity -> String
buildImplementation e = final
                      where
                      declaration = "@implementation " ++ (entityName e) ++ " (fetch)\n"

                      requests = [buildQueryImplementation x y | x <- (entityAttributes e), y <- [e]]
                      end = "@end\n\n"
                      final = declaration ++ (concat requests) ++ end

buildQueryDeclaration :: Attribute -> String
buildQueryDeclaration a = concat ["+ (NSArray *)" ++ y ++ z ++ signature ++ ";\n\n" | (_, z) <- operators, y <- [attrName a]]

buildQueryImplementation :: Attribute -> Entity -> String
buildQueryImplementation a e = concat ["+ (NSArray *)" ++ y ++ z ++ signature ++ " {\n" ++ (fetchRequest e)  ++ (setPredicate a x) ++ sortDescriptor ++ errorVar ++ executeFetch ++ errorBlock ++ "\treturn results;" ++ "\n}\n\n" | (x, z) <- operators, y <- [attrName a]]
