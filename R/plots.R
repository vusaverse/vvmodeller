#' Barplot per level
#'
#' Maakt een barplot per level van een factor.
#'
#' @param x een categoriale vector
#' @export
barplot_per_level <- function(x) {
  ## Controleer of graphics is geinstalleerd
  vvmover::check_installed_package("graphics")
  graphics::barplot(prop.table(table(x)))
}

#' SA Category Plot
#'
#'
#' @param data De data frame
#'
#' @param x Geef de waarde op voor x
#' @param y Geefde waarde op voor y
#' @param title Indien geen titel wordt opgegeven, automatisch titel
#' adv van 'x' en 'y'.
#' @param remove_labels Als het aantal categorieen hoger is dan remove_labels, wordt
#' het label verwijderd
#' @export
sa_category_plot <- function(data, x, y, title, remove_labels = 10) {
  aes_string <- NULL
  ## Controleer of gridExtra, grid en ggplot2 zijn geinstalleerd
  vvmover::check_installed_package("gridExtra")
  vvmover::check_installed_package("grid")
  vvmover::check_installed_package("ggplot2")

  ## Als de titel ontbreekt wordt deze ingevuld
  if (missing("title")) {
    title <- paste(x, y, sep = "/")
  }

  ## Een boxplot per categorie
  plot_box <-
    ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y)) +
    ggplot2::geom_boxplot(width = 0.5,
                          alpha = 0.9,
                          fill = VU_BlueLight) +
    # Verwijder het X-label: die staat al in de titel
    ggplot2::theme(axis.title.x = ggplot2::element_blank())

  ## Een histogram per categorie
  plot_hist <- ggplot2::ggplot(data, aes_string(x = x)) +
    ggplot2::geom_histogram(stat = "count") +
    # Verwijder het X-label: die staat al in de titel
    ggplot2::theme(axis.title.x = ggplot2::element_blank())

  ## Als het aantal categorieen hoger is dan remove_labels, wordt
  ## het label verwijderd.
  if (length(unique(data[[x]])) > remove_labels) {
    plot_box <- plot_box +
      ggplot2::theme(axis.text.x = ggplot2::element_blank())

    plot_hist <- plot_hist +
      ggplot2::theme(axis.text.x = ggplot2::element_blank()) +
      ggplot2::labs(caption = "Teveel categorieen, labels worden niet getoond")

  }

  ## Voeg samen in een figuur
  gridExtra::grid.arrange(plot_box,
                          plot_hist,
                          top = grid::textGrob(title,
                                               gp = grid::gpar(fontsize = 15)))

}

#' SA Dotchart
#'
#' Maakt een Dotchart.
#'
#' @param data De data frame
#' @param var Een character string
#' @export
sa_dotchart <- function(data, var) {
  ## Controleer of ggplot2 is geinstalleerd
  vvmover::check_installed_package("ggplot2")
  ggplot2::ggplot(data = data, ggplot2::aes(y = as.numeric(row.names(data)))) +
    ggplot2::geom_point() +
    ggplot2::aes_string(x = var)
}

#' SA Histogram
#'
#' @param data De data frame.
#'
#' @param var Een character string
#'
#'@export
sa_hist <- function(data, var) {
  ## Controleer of ggplot2 is geinstalleerd
  vvmover::check_installed_package("ggplot2")
  ggplot2::ggplot(data = data) +
    ggplot2::geom_histogram() +
    ggplot2::aes_string(x = var)
}

#' SA Dotchart Histogram
#'
#' Maakt een Dotchart en Histogram en voegt deze samen in een figuur.
#'
#' @param data De data frame
#'
#' @param var Een character string
#' @param title Indien geen titel wordt opgegeven, automatisch titel gegenereerd adv var naam
#'
#'@export
sa_dotchart_hist <- function(data, var, title = NULL) {
  ## Controleer of ggplot2 en grid zijn geinstalleerd
  vvmover::check_installed_package("grid")
  vvmover::check_installed_package("ggplot2")
  ## Als de titel ontbreekt wordt deze ingevuld
  if (missing("title")) {
    title <- paste("Spreiding van", var)
  }

  ## Maak de dotchart
  dot <- sa_dotchart(data, var) +
    # Verwijder het X-label: die staat al in de titel
    ggplot2::theme(axis.title.x = ggplot2::element_blank())
  ## Maak de historgram
  hist <- sa_hist(data, var) +
    # Verwijder het X-label: die staat al in de titel
    ggplot2::theme(axis.title.x = ggplot2::element_blank())
  ## Voeg samen in een figuur

  gridExtra::grid.arrange(dot,
                          hist,
                          top = grid::textGrob(title,
                                               gp = grid::gpar(fontsize = 15)))
}


#' SA boxplot
#'
#' Wrapper functie rondom boxplot uit package 'graphics'
#'
#' @param x Een vector of string
#'
#' @param col Kleur om default VU kleur (VU_BlueLight) te overrulen.
#' @param ... De overige parameters worden meegegeven aan de functie.
#'
#'@export
sa_boxplot <- function(x, col = VU_BlueLight, ...) {
  ## Controleer of graphics is geinstalleerd
  vvmover::check_installed_package("graphics")
  # Defaults
  # x, ..., range = 1.5, width = NULL, varwidth = FALSE,
  # notch = FALSE, outline = TRUE, names, plot = TRUE, border = par("fg"),
  # col = NULL, log = "", pars = list(boxwex = 0.8, staplewex = 0.5,
  #                                   outwex = 0.5), horizontal = FALSE, add = FALSE, at = NULL
  graphics::boxplot(x, col = c(col),
                    ...)
}


#' SA dotchart old
#'
#' Wrapper functie rondom dotchart uit package 'graphics'
#'
#' @param x Een string of vector.
#'
#' @param lcolor Kleur om default VU kleur (VU_BlueLight) te overrulen.
#' @param ... De overige parameters wroden meegegeven aan de functie.
#'
#'@export
sa_dotchart_old <- function(x, lcolor = VU_BlueLight, ...) {
  ## Controleer of graphics is geinstalleerd
  vvmover::check_installed_package("graphics")
  # Defaults
  # x, labels = NULL, groups = NULL, gdata = NULL, cex = par("cex"),
  # pt.cex = cex, pch = 21, gpch = 21, bg = par("bg"), color = par("fg"),
  # gcolor = par("fg"), lcolor = "gray", xlim = range(x[is.finite(x)]),
  # main = NULL, xlab = NULL, ylab = NULL, ...
  graphics::dotchart(x, lcolor = lcolor,
                     ...)
}


#' SA violinplot
#'
#' Wrapper functie rondom violinplot.
#'
#' @param df De data frame
#'
#' @param variable Een character string
#' @param fill_violin Kleur van violin staat standaard op "VU_BlueMiddle"
#' @param fill_boxplot Kleur van boxplot staat standaard op "VU_Bluelight"
#'
#'@export
sa_violinplot <-
  function(df,
           variable,
           fill_violin = VU_BlueMiddle,
           fill_boxplot = VU_BlueLight) {
    ## Controleer of ggplot2 is geinstalleerd
    vvmover::check_installed_package("ggplot2")
    # Defaults
    plot <- ggplot2::ggplot(df,
                            ggplot2::aes(x = 1, y = variable)) +
      ggplot2::geom_violin(fill = fill_violin) +
      ggplot2::geom_boxplot(width = 0.5,
                            alpha = 0.4,
                            fill = fill_boxplot)
    return(plot)
  }

#' Plot model variabelen.R
#'
#' Doel: De functie plot_model_variabelen maakt explorerende plots voor de
#' variabelen in het opgegeven model.
#'
#' Afhankelijkheden: Index.R
#'
#' Opmerkingen:
#' 1) Plots worden weggeschreven naar
#'    /Datasets/Plots/Exploreren/(y)/(modelversie)/(Datum)
#'
#' @param data Dataframe
#' @param y Y-variabele van het model (string)
#' @param x Vector met X-variabelen (strings)
#' @param modelversie Stringtekst om te bepalen waar de plots worden weggeschreven
#' @importFrom dplyr %>%
#' @export
plot_model_variabelen <- function(data, y, x, modelversie){
  ggsave <- dev.copy <- png <- dev.off <- Network_directory <- select <- one_of <- select_if <- contains <- vars <- filter
  q_na <- par <- SA_dotchart <- text <- ggplot <- aes_string <-geom_violin <- geom_boxplot <- geom_histogram <- ggtitle <- ylab <- xlab <- geom_jitter <- geom_smooth <-
    lm <- continu <- filter <- NULL


  ## Functies om plots op te slaan in de juiste folder
  ## Deze functie kan achter een ggplot gezet worden met gebruik van "+"
  SA_save_ggplot <- function(name = "", ...){
    ggsave(paste0(Output_directory_plots, name ,".png"),
           width = 12,
           height = 9,
           units = "in")
  }
  ## Deze functie kan je gebruiken om de plot die zojuist gemaakt is op te slaan
  SA_save_plot <- function(name = ""){
    dev.copy(png, filename=paste0(Output_directory_plots, name ,".png"))
    dev.off()
  }


  Y_variabele  <- y
  X_variabelen <- x
  dfM          <- data
  ## Maak de Output directory aan
  Output_directory_plots <- paste(Network_directory, "Plots/Exploreren", Y_variabele, modelversie, Sys.Date(), "", sep = "/")

  #dir.create(Output_directory_plots, recursive=TRUE)


  ## Maak een vector met alle continue X-variabelen (behalve als dummy in de naam zit)
  continu <- dfM %>%
    select(one_of(X_variabelen)) %>%
    select_if(is.numeric) %>%
    select(-contains("dummy")) %>%
    names()

  print("Continue variabelen")
  print(continu)

  ## Alle andere variabelen zijn factors, daarvan wordt de categoriale vector
  categoriaal <- X_variabelen[!X_variabelen %in% continu]
  print("Categoriale variabelen")
  print(categoriaal)

  ## Transformeer alle categoriale variabelen naar factor
  dfM <- dfM %>% dplyr::mutate_at(vars(one_of(categoriaal)), list(factor))

  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## XX. Data set inspecteren
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  inspect <- vvsculptor::df_inspect(dfM, print_results = F)

  print("Variabelen met meer dan 10 NA's")
  print(filter(inspect, q_na > 10))

  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
  ## 2. PROTOCOL TOEPASSEN
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Data exploratie heeft 8 onderdelen (conform ZUUR):
  # A. 1) Outliers in Y & 2) Outliers in X
  # B. Homogeniety
  # C. Normality
  # D. Zero inflation Y
  # E. Collinearity X
  # F. Relationships Y vs X
  # G. Interactions (is the quality of the data good enough to include them?)
  # H. Are categorical covariates balanced?

  ## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  ## A. OUTLIERS

  ## _____________________________________________________________________________
  ## A.1 Outliers in Y

  ## Maak ruimte voor 1 rij, met 2 afbeeldingen
  par(mfrow = c(1, 2))

  ## Maak een boxplot en Cleveland dotplot
  sa_boxplot(x = dfM[,Y_variabele])

  SA_dotchart(x = dfM[,Y_variabele],
              xlab = "Waarden van de variabele",
              ylab = "Volgorde van de data")
  text(sa_line2user(line = mean(par('mar')[c(2, 4)]), side=2),
       sa_line2user(line = 2, side = 3),
       Y_variabele,
       xpd = NA,
       cex = 1,
       font = 2,
       col = VU_BlueMiddle)
  #SA_save_plot(name = "A.1. Outliers in Y")
  ## _____________________________________________________________________________
  ## A.2 Outliers in X

  ## Maak per variabele een dotchart;
  ## Dit wordt voor iedere continue variabele gedaan.
  par(mfrow = c(1, 1))
  for(name in continu) {
    SA_dotchart(dfM[,name], main = name)
    #SA_save_plot(name = paste("A.2. Outliers in X Dotchart",name))
    print(paste("A.2. Outliers in X Dotchart",name))
  }


  ## Maak voor iedere categoriale variabele een boxplot tov de Y
  par(mfrow = c(1, 1))
  for(name in categoriaal) {
    plot <- ggplot(dfM, aes_string(x=name, y=Y_variabele)) +
      geom_violin(fill=VU_BlueMiddle, alpha = 0.1) +
      geom_boxplot(width = 0.5, alpha = 0.9, fill = VU_BlueLight) #+
    #SA_save_ggplot(paste("B.4. Boxplots X Y", name))
  }

  ## Apart een plot voor opleiding maken omdat die teveel categorieen heeft om goed weer te geven
  #ggplot(dfM, aes(x=1, y=Jaren_tot_diploma)) +
  #    geom_violin(fill=VU_BlueMiddle) +
  #    geom_boxplot(width = 0.5, alpha = 0.4, fill = VU_BlueLight) + facet_wrap(~ Opleiding, nrow=6)+
  #    theme(axis.line=element_blank(),
  #          axis.text.x=element_blank(),
  #          axis.title.x=element_blank()) +
  #    SA_save_ggplot("B.4. Boxplots XY Opleiding")




  ## _____________________________________________________________________________
  ## D. ZERO INFLATION
  ## Hoeveel nullen bevat de dataset

  # In case of extremely high number of zeros: we should not apply an
  # ordinary Poisson or negative binomial GLM as these would produce biased
  # parameter estimates and standard errors. Instead one should consider zero
  # inflated GLM.

  # D1. Tel het aantal nullen en geef een verhouding weer tot het totaal
  # sum(dfM$Jaren_tot_diploma == 0)
  # sum(dfM$Jaren_tot_diploma == 0) / nrow(dfM)

  # Uitkomst: 0

  # D2. Plot een frequentietabel
  ggplot(dfM, aes_string(Y_variabele)) +
    geom_histogram(fill=VU_BlueMiddle, binwidth = 0.2 )+
    #scale_x_continuous(breaks = seq(0, 10, by = 0.5))+
    ggtitle("D. Zero inflation - Y") +
    ylab("Aantal") +
    xlab(Y_variabele)# +
  #SA_save_ggplot("D.2. Zero inflation frequentietabel")

  ## _____________________________________________________________________________
  ## E. COLLINEARITY

  ## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  ## E.2 VIF - Variance inflation factors
  ## Bereken nu de vif scores voor dit model


  ## E.2. CORRELATION MATRIX
  ## Correlatie matrix voor de continue variabelen
  GGally::ggcorr(select(dfM, one_of(continu)),  method = c("na.or.complete", "spearman"), label = T, label_size = 3, label_alpha = T,  hjust = .85, size = 3,
                 layout.exp=2) #+
  #SA_save_ggplot("E.2. Correlation matrix")

  ## _____________________________________________________________________________
  ## F. RELATIONSHIP X <> Y

  # Another essential part of data exploration, especially in
  # univariate analysis, is plotting the response variable vs. each
  # covariate
  # Besides visualizing relationships between variables, scatterplots
  # are also useful to detect observations that do not comply
  # with the general pattern between two variables
  # Any observation that sticks out from the black cloud
  # needs further investigation

  ## Relatie tussen numerieke variabelen en jaren tot diploma + GAM smoother lijn. Eventueel te wijzigen in GLM lijn in geom_smooth
  SA_relation <- ggplot(dfM, aes_string(y=Y_variabele)) + geom_jitter(alpha=0.2) + geom_smooth()
  for(name in continu) {
    SA_relation + aes_string(x = name) #+
    # SA_save_ggplot(paste("F. Scatterplot X-Y", name))
  }

  ## _____________________________________________________________________________
  ## G. INTERACTIONS
  # if all lines are parallel,then there is probably no significant interaction
  # (although only the regression analysis can tell us whether this is indeed the case)

  ## Relatie numerieke x variabelen plotten tegen categoriale variabele Geslacht en Alfa_Beta_Gamma
  ## Eventueel ook uitvoeren met andere categoriale variabelen zoals Uitgeloot, Verlengd BSA, etc.

  ## Maak de basisplot
  SA_interact <- ggplot(dfM, aes_string(y=Y_variabele)) +
    geom_jitter(alpha=0.2) +
    geom_smooth(method=lm)

  ## Maak een plot voor iedere categorie<>continue combinatie
  for(cat in categoriaal){
    for(con in continu){
      SA_interact +
        aes_string(col = cat, x=con) #+
      # SA_save_ggplot(paste("G. Interactie", cat, con))
    }
  }
}



