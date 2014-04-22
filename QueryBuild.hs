module QueryBuild
(
 buildDeclaration
,fileName
) where
import CoreDataHs
fileName :: Entity -> String
fileName e = (entityName e) ++ "_Fetcher.h"

buildDeclaration :: Entity -> String
buildDeclaration e = final
                      where                      
                      imports = "#import <CoreData/CoreData.h>\n"
                                   ++ "#import <Foundation/Foundation.h>\n"
                      declaration = "@interface" ++ (entityName e) ++ " (fetch)\n"

                      requests = map buildQueries (entityAttributes e)
                      end = "@end\n\n"
                      final = imports ++ declaration ++ (concat requests) ++ end


buildQueries :: Attribute -> String
buildQueries a = "- (void)fetch" ++ (attrName a) ++ ";" ++ "\n"
