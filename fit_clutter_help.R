data("ex12_mfr")

head(ex12_mfr,15)

fit_clutter(ex12_mfr, "Idade", "HD", "B", "V", "Site", 
            .groups1 = c("Talhao", "Parcela"), model=1)

fit_clutter(ex12_mfr, "Idade", "HD", "B", "V", "Site", 
            .groups1 = c("Talhao", "Parcela"), model=2)

fit_clutter(ex12_mfr, "Idade", "HD", "B", "V", "Site", 
            .groups1 = c("Talhao", "Parcela"),
            .groups2 = "Talhao" )

fit_clutter(ex12_mfr, "Idade", "HD", "B", "V", "Site", 
            .groups1 = c("Talhao", "Parcela"), model=1)


struct_form(ex12_mfr, "Idade", "HD", "B", "V", "Site", c("Talhao", "Parcela") ) 

struct_form(ex12_mfr,"Idade", "HD", "B", "V", "Site", c("Talhao", "Parcela") ) %>% 
  fit_clutter(struct_form_df=T)
