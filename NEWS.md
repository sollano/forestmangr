# forestmangr development

# forestmangr 0.9.1

* Graybill_F is now graybill_f;

* removed unnecessary imports;

* updated description;

* converted all exemple data to tibbles;

* removed head command from examples, since it's not necessary anymore;

* fixed some typos;

* added a gray_scale argument to est_clutter function;

* fixed a bug where if a dataframe cointaned NA, resid_plot would fail;

* vol_summarise now outputs ungrouped data;

* fixed vol_summarise name for average form factor value;

* fixed the example of lm_table;

* fixed the example of strs; number of plots needed to be sampled was wrong;

* changed exfm19 to a data by tree;

* added 6 vignettes in portuguese;

* added 6 vignettes in english;

* tree_summarise is now improved, and keeps all variables from data;

* strs can now handle special characters;

* added a new example data, with revenue values for a eucalyptus forest;

* added a new function, npv_irr, for net present value, irr calculation and sensibility evaluation;

* fixed a bug in huberwob, which made a wrong call to df;

* fixed bugs in various functions, with a call for is.na with a 2 length vector.

# forestmangr 0.9.0

* release