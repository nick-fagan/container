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

%%  data need not be numeric

cont.data = cell( cont.shape() );
cont.data = struct( 'salaries', {40e3; 50e3; 60e3} );

%%  iteration

objs = cont.enumerate( 'people' );

%%  obtain and assign fields of labels

people = cont( 'people' );
cont( 'countries' ) = 'all__countries';

%%  select certain rows

subset = cont(1:2);

%%  remove

removed = cont.remove( {'mark', 'la'} );

%%  partition + recombine

part1 = cont.only( {'mark', 'brent'} );
part1 = part1.replace( {'mark', 'brent'}, 'group1' );
part2 = cont.only( 'james' );
part2 = part2.replace( 'james', 'group2' );

recombined = part1.append( part2 );