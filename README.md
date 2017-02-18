`Container` is a MATLAB data structure that enables simple and semantic data partitioning, recombination, transformation, and iteration.

A `Container` has `data` and `labels`; each row of `data` is identified by a set of `labels` separated into fields / categories.

This makes data searchable – 

~~~~
index = container.where( ‘some_label’ );
~~~~

– partionable –

~~~~
separated = container.only( ‘some_label’ );
~~~~

– transformable –

~~~~
relabeled = container.replace( {‘some_label’, ‘some_other_label}, ‘some_new_label’ );
~~~~

– and iterable –

~~~~
array_of_objects = container.enumerate( {‘some_category’, ‘some_other_category’} );
~~~~

//

`example.mat` should give a feel for how inputs must be structured.

For best performance, it is recommended to call `sparse()` after instantiation – this converts the labels object to a `SparseLabels` object, which takes advantage of sparse matrices in MATLAB.