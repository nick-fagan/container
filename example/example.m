%%  instantiate

%   fieldnames of `labels` are fields / categories. 
  
labels.people = { 'mark'; 'james'; 'brent' };
labels.cities = { 'ny'; 'la'; 'sf' };
labels.countries = { 'us'; 'canada'; 'india' };

data = rand( numel(labels.people), 5 );

cont = Container( data, labels );
%   improve performance by reformatting the labels object into an array
%   of sparse logical indices.
cont = cont.sparse();

disp( cont.only({'mark', 'brent'}) );

%%  iteration

objs = cont.enumerate( 'people' );

%%  obtain a category of labels

people = cont( 'people' );

%%  select certain rows

subset = cont(1:2);

%%  remove

removed = cont.remove( {'mark', 'la'} );