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
fetchRequest (Entity name _ _) = "\tNSFetchRequest *fetchRequest = [NSFetchRequest fetchRequestWithEntityName:" 
                                  ++ "@\"" 
                                  ++ name 
                                  ++ "\"];\n"

setPredicate :: Attribute -> String -> String
setPredicate a s = "\t[fetchRequest setPredicate:[NSPredicate predicateWithFormat:" 
                    ++ "@\"" 
                    ++ attrName a 
                    ++  " " 
                    ++ s 
                    ++ " %@\", " 
                    ++ "object]];\n"

sortDescriptor :: String
sortDescriptor = "\t[fetchRequest setSortDescriptors:sort];\n"

errorVar :: String
errorVar = "\tNSError *err = nil;\n"

executeFetch :: String
executeFetch = "\tNSArray *results = [context executeFetchRequest:fetchRequest error:&err];\n"

errorBlock :: String
errorBlock = "\tif(!result && errorBlock) {\n" 
              ++ "\t\terrorBlock(err);\n" 
              ++ "\t\treturn nil;\n" 
              ++ "\t}\n"

signature :: String
signature = ":(id)object " 
            ++ "inContext:(NSManagedObjectContext *)context " 
            ++ "sortDescriptors:(NSArray *)sort" 
            ++  " error:(void(^)(NSError *error))errorBlock"

intFileName :: Entity -> String
intFileName (Entity name _ _) = name ++ "_Fetcher.h"

impFileName :: Entity -> String
impFileName (Entity name _ _) = name ++ "_Fetcher.m"

buildDeclaration :: Entity -> String
buildDeclaration (Entity name attributes _) = final
                      where
                      imports = "#import <CoreData/CoreData.h>\n"
                                   ++ "#import <Foundation/Foundation.h>\n"
                      declaration = "@interface " ++ name ++ " (Fetcher)\n\n"
                      requests = map buildQueryDeclaration attributes
                      stringDec = map buildQueryStringDeclaration attributes
                      end = "@end\n\n"
                      final = imports ++ declaration ++ concat requests ++ concat stringDec ++ end


buildImplementation :: Entity -> String
buildImplementation e@(Entity name attributes _) = final
                      where
                      declaration = "@implementation " ++ name ++ " (Fetcher)\n"
                      requests = [buildQueryImplementation x e | x <- attributes]
                      stringReq = [buildQueryStringImplementation x e | x <- attributes]
                      end = "@end\n\n"
                      final = declaration ++ concat requests ++ concat stringReq ++ end

buildQueryDeclaration :: Attribute -> String
buildQueryDeclaration (Attribute name _ _) = concat ["+ (NSArray *)" 
                                                      ++ name 
                                                      ++ z 
                                                      ++ signature 
                                                      ++ ";\n\n" 
                                                      | (_, z) <- normalOperators]

buildQueryStringDeclaration :: Attribute -> String
buildQueryStringDeclaration (Attribute name t _) = concat ["+ (NSArray *)" 
                                                              ++ name 
                                                              ++ z 
                                                              ++ signature 
                                                              ++ ";\n\n" 
                                                              | (_, z) <- stringOperators, t == "String"]

buildQueryImplementation :: Attribute -> Entity -> String
buildQueryImplementation a@(Attribute name _ _) e = concat ["+ (NSArray *)" 
                                                            ++ name 
                                                            ++ z 
                                                            ++ signature 
                                                            ++ " {\n" 
                                                            ++ fetchRequest e  
                                                            ++ setPredicate a x 
                                                            ++ sortDescriptor 
                                                            ++ errorVar 
                                                            ++ executeFetch 
                                                            ++ errorBlock 
                                                            ++ "\treturn results;" 
                                                            ++ "\n}\n\n" 
                                                            | (x, z) <- normalOperators]

buildQueryStringImplementation :: Attribute -> Entity -> String
buildQueryStringImplementation a@(Attribute name t _) e = concat ["+ (NSArray *)" 
                                                                  ++ name 
                                                                  ++ z 
                                                                  ++ signature 
                                                                  ++ " {\n" 
                                                                  ++ fetchRequest e 
                                                                  ++ setPredicate a x 
                                                                  ++ sortDescriptor 
                                                                  ++ errorVar 
                                                                  ++ executeFetch 
                                                                  ++ errorBlock 
                                                                  ++ "\treturn results;" 
                                                                  ++ "\n}\n\n" 
                                                                  | (x, z) <- stringOperators, t == "String"]
