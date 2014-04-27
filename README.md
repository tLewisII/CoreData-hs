# CoreData-hs

Generates fetch requests for the entities and properties in a CoreData Model
file. Outputs methods on a category of your entities that look like this:
```
+ (NSArray *)nameIsEqualTo:(id)object inContext:(NSManagedObjectContext *)context sortDescriptors:(NSArray *)sort error:(void(^)(NSError *error))error {
	NSFetchRequest *fetchRequest = [NSFetchRequest fetchRequestWithEntityName:@"TJLSuperEntity"];
	[fetchRequest setPredicate:[NSPredicate predicateWithFormat:@"name = %@", object]];
	[fetchRequest setSortDescriptors:sort];
	NSError *err = nil;
	NSArray *results = [context executeFetchRequest:fetchRequest error:&err];
	if(err && error) {
		error(err);
		return nil;
	}
	return results;
}
```
Stringly typed code is bad, and this does not get rid of it, but it at
least has a machine generate it, which makes it more reliable than typing it
out by hand.

## Installation

The easiest way to currently "install" CoreData-hs is to download the executable in the distribution folder. Or, if you want you can download and build it from the source. It is written in [Haskell](http://haskell.org), and you will need the ghc compiler to build the executable.

## Usage

    $ ./CoreDataHs [ModelFileName]
Simply pass the name of the Model file you wish to generate requests for, without any extensions and Fetcher will do the rest.

## Options

Currently, the only options available are -V or --version to get the version number.

## License

Copyright © 2014 Terry Lewis II

Distributed under the MIT License
