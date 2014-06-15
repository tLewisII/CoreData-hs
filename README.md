# CoreData-hs

Generates fetch requests for the entities and properties in a CoreData Model
file. Given an Entity `Person` with a `name` and `age` property, the output for the .h would be as follows:

```
#import <CoreData/CoreData.h>
#import <Foundation/Foundation.h>
@interface Person (Fetcher)

+ (NSArray *)ageIsEqualTo:(id)object inContext:(NSManagedObjectContext *)context sortDescriptors:(NSArray *)sort error:(void(^)(NSError *error))errorBlock;

+ (NSArray *)ageIsLessThan:(id)object inContext:(NSManagedObjectContext *)context sortDescriptors:(NSArray *)sort error:(void(^)(NSError *error))errorBlock;

+ (NSArray *)ageIsGreaterThan:(id)object inContext:(NSManagedObjectContext *)context sortDescriptors:(NSArray *)sort error:(void(^)(NSError *error))errorBlock;

+ (NSArray *)ageIsGreaterThanOrEqualTo:(id)object inContext:(NSManagedObjectContext *)context sortDescriptors:(NSArray *)sort error:(void(^)(NSError *error))errorBlock;

+ (NSArray *)ageIsLessThanOrEqualTo:(id)object inContext:(NSManagedObjectContext *)context sortDescriptors:(NSArray *)sort error:(void(^)(NSError *error))errorBlock;

+ (NSArray *)ageIsNotEqualTo:(id)object inContext:(NSManagedObjectContext *)context sortDescriptors:(NSArray *)sort error:(void(^)(NSError *error))errorBlock;

+ (NSArray *)ageIsBetwixt:(id)object inContext:(NSManagedObjectContext *)context sortDescriptors:(NSArray *)sort error:(void(^)(NSError *error))errorBlock;

+ (NSArray *)nameIsEqualTo:(id)object inContext:(NSManagedObjectContext *)context sortDescriptors:(NSArray *)sort error:(void(^)(NSError *error))errorBlock;

+ (NSArray *)nameIsLessThan:(id)object inContext:(NSManagedObjectContext *)context sortDescriptors:(NSArray *)sort error:(void(^)(NSError *error))errorBlock;

+ (NSArray *)nameIsGreaterThan:(id)object inContext:(NSManagedObjectContext *)context sortDescriptors:(NSArray *)sort error:(void(^)(NSError *error))errorBlock;

+ (NSArray *)nameIsGreaterThanOrEqualTo:(id)object inContext:(NSManagedObjectContext *)context sortDescriptors:(NSArray *)sort error:(void(^)(NSError *error))errorBlock;

+ (NSArray *)nameIsLessThanOrEqualTo:(id)object inContext:(NSManagedObjectContext *)context sortDescriptors:(NSArray *)sort error:(void(^)(NSError *error))errorBlock;

+ (NSArray *)nameIsNotEqualTo:(id)object inContext:(NSManagedObjectContext *)context sortDescriptors:(NSArray *)sort error:(void(^)(NSError *error))errorBlock;

+ (NSArray *)nameIsBetwixt:(id)object inContext:(NSManagedObjectContext *)context sortDescriptors:(NSArray *)sort error:(void(^)(NSError *error))errorBlock;

+ (NSArray *)nameIsLike:(id)object inContext:(NSManagedObjectContext *)context sortDescriptors:(NSArray *)sort error:(void(^)(NSError *error))errorBlock;

+ (NSArray *)nameContains:(id)object inContext:(NSManagedObjectContext *)context sortDescriptors:(NSArray *)sort error:(void(^)(NSError *error))errorBlock;

+ (NSArray *)nameMatches:(id)object inContext:(NSManagedObjectContext *)context sortDescriptors:(NSArray *)sort error:(void(^)(NSError *error))errorBlock;

+ (NSArray *)nameBeginsWith:(id)object inContext:(NSManagedObjectContext *)context sortDescriptors:(NSArray *)sort error:(void(^)(NSError *error))errorBlock;

+ (NSArray *)nameEndsWith:(id)object inContext:(NSManagedObjectContext *)context sortDescriptors:(NSArray *)sort error:(void(^)(NSError *error))errorBlock;

@end

```
With the implementation looking like this:
```
+ (NSArray *)nameIsEqualTo:(id)object inContext:(NSManagedObjectContext *)context sortDescriptors:(NSArray *)sort error:(void(^)(NSError *error))errorBlock {
	NSFetchRequest *fetchRequest = [NSFetchRequest fetchRequestWithEntityName:@"TJLSuperEntity"];
	[fetchRequest setPredicate:[NSPredicate predicateWithFormat:@"name = %@", object]];
	[fetchRequest setSortDescriptors:sort];
	NSError *err = nil;
	NSArray *results = [context executeFetchRequest:fetchRequest error:&err];
	if(!results && errorBlock) {
		error(err);
		return nil;
	}
	return results;
}
```

Note that I only show one method here, because the .m for this very small `Person` Entity is almost 300 lines long. :)

So basically, Stringly typed code is bad, and this does not get rid of it, but it at
least has a machine generate it, which makes it more reliable than typing it
out by hand.

## Installation

The easiest way to currently "install" CoreData-hs is to download the executable in the "distribution" folder. Or, if you want you can download and build it from the source. It is written in [Haskell](http://haskell.org), and you will need the ghc compiler to build the executable.

## Usage

`$ ./coredata-hs [ModelFileName]`
Simply pass the name of the Model file you wish to generate requests for, without any extensions and CoreData-hs will do the rest.
You can also drop the executable in `usr/local/bin` if you want to be able to run it from anywhere.

## Options

`$ ./coredata-hs -V` or `$ ./coredata-hs --version` returns the current version. Or, if you want to generate requests for a single entity, rather than all of them do `$ ./coredata-hs ModelFileName --entity EntityName` and only requests for the given entity will be created.

## License

Copyright © 2014 Terry Lewis II

Distributed under the MIT License
