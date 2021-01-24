# forestmangr development

# forestmangr 0.9.3

* replace order function call with dplyr's arrange, due to order's behaviour in certain situations;

* add shiny and miniUI dependency;

* add lm_resid and lm_resid_group experimental functions. These output shiny gadgets;

* changes in sprs,ss_dffs,classify_site,dom_height,fit_clutter,plot_summarise,smalianwb,smalianwob,tree_summarise due to dplyr updates (due to .add argument);


# forestmangr 0.9.2

* updated nls_table and lm_table to work with tibble 3.0.0;

* new function: rm_empty_cols; this function is used internally to better handle optional variables;

* added a new dependency, forcats;

* new function: ident_model_full; this function runs the complete identity model test;

* fixed a bug where lm_table and nls_table would sometimes change the order of coefficients;

* graybill_f now accepts vectors as input;

* average_tree_curve now has a color argument, that allows variables to be mapped as colors;

* average_tree_curve now plots a mirrored plot, as to resemble a tree (mirror argument) and has an argument to remove the equation from the plot;

* rmse_perc and bias_perc now have a na.rm argument (which is TRUE by default);

* updated tree_summarise to sum volume with and without bark using 2 new arguments;

* fixed vol_summarise handling of optional variables;

* new function: class_center;

* diameter_class and bdq_meyer can now handle all values of class intervals correctly.

# forestmangr 0.9.1

* Graybill_F is now graybill_f;

* removed unnecessary imports;

* updated description;

* converted all example data to tibbles;

* removed head command from examples, since it's not necessary anymore;

* fixed some typos;

* added a gray_scale argument to est_clutter function;

* fixed a bug where if a dataframe contained NA, resid_plot would fail;

* vol_summarise now outputs ungroup data;

* fixed vol_summarise name for average form factor value;

* fixed the example of lm_table;

* fixed the example of strs; number of plots needed to be sampled was wrong;

* changed exfm19 to a data by tree;

* added 6 vignettes in Portuguese;

* added 6 vignettes in English.

* tree_summarise is now improved, and keeps all variables from data;

* strs can now handle special characters;

* added a new example data, with revenue values for a eucalyptus forest;

* added a new function, npv_irr, for net present value, irr calculation and sensibility evaluation;

* fixed a bug in huberwob, which made a wrong call to df;

* fixed bugs in various functions, with a call for is.na with a 2 length vector.

# forestmangr 0.9.0

* release