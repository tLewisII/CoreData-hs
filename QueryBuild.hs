module QueryBuild
(
 buildDeclaration
,buildImplementation
,intFileName
,impFileName
) where
import CoreDataHs
operators :: [(String, String)]
operators = [
            ("=", "isEqualTo"),
            ("<", "isLessThan"),
            (">", "isGreaterThan"),
            (">=", "isGreaterThanOrEqualTo"),
            ("<=", "isLessThanOrEqualTo")
            ]
signature :: String
signature = ":(id)object " ++ "inContext:(NSManagedObjectContext *)context " ++ "error:(NSError **)error"

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
                      imports = "#import <CoreData/CoreData.h>\n"
                                   ++ "#import <Foundation/Foundation.h>\n"
                      declaration = "@implementation " ++ (entityName e) ++ " (fetch)\n"

                      requests = map buildQueryImplementation (entityAttributes e)
                      end = "@end\n\n"
                      final = imports ++ declaration ++ (concat requests) ++ end

buildQueryDeclaration :: Attribute -> String
buildQueryDeclaration a = concat ["- (NSArray *)" ++ y ++ z ++ signature ++ ";\n\n" | (_, z) <- operators, y <- [attrName a]]

buildQueryImplementation :: Attribute -> String
buildQueryImplementation a = concat ["- (void)" ++ y ++ z ++ signature ++ " {\n\n" ++ "}\n" | (x, z) <- operators, y <- [attrName a]]
--"- (void)" ++ (attrName a) ++ operators ++ " {" ++ "\n\n"
--                          ++ "}\n\n"
