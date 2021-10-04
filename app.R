# License and Copyright Information
#
# This Premorbid IQ Simulator Shiny App by Brandon Gavett is licensed under a 
# Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
# Based on work at https://github.com/begavett/simulate-piq
# and published in Psychological Assessment.
# Copyright 2021 Brandon Gavett
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(MASS)
library(dplyr)
library(data.table)
library(xtable)
library(mirt)

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Simulate premorbid IQ and cognitive test data"),

    # Sidebar with numeric inputs 
    sidebarLayout(
        sidebarPanel(
            numericInput("theta_piq_ss",
                        "Population mean for IQ (Standard Score units; M = 100, SD = 15):",
                        min = 70,
                        max = 130,
                        value = 100,
                        step = 1),
            numericInput("rho_piq_cog",
                         "Population correlation between pIQ and cognitive test:",
                         min = 0,
                         max = 1,
                         value = .44,
                         step = .01),
            numericInput("z_cutoff",
                         "Cutoff for classifying a score as abnormal (number of standard deviations from the reference):",
                         min = -3,
                         max = -.01,
                         value = -1,
                         step = .01),
            numericInput("br_cd",
                         "Base rate of cognitive disorder in the population:",
                         min = .05,
                         max = .95,
                         value = .4,
                         step = .01),
            numericInput("delta_cd",
                         "Population effect size of cognitive disorder on cognitive test (number of standard deviations from the mean):",
                         min = -3,
                         max = -.01,
                         value = -.70,
                         step = .01),
            numericInput("sim_n",
                         "Number of cases to simulate (higher = more precise estimates, but takes longer):",
                         min = 100,
                         max = 1e07,
                         value = 1e06,
                         step = 100000),
            numericInput("edu",
                         "Years of education:",
                         min = 0,
                         max = 20,
                         value = 12,
                         step = 1),
            numericInput("icr_cog",
                         "Internal consistency reliability of cognitive test:",
                         min = .01,
                         max = .99,
                         value = .90,
                         step = .01),
            numericInput("truncate_cog_lo",
                         "Minimum possible score on cognitive test (Standard Score units):",
                         min = 10,
                         max = 85,
                         value = 40,
                         step = 1),
            numericInput("truncate_cog_hi",
                         "Maximum possible score on cognitive test (Standard Score units):",
                         min = 115,
                         max = 190,
                         value = 160,
                         step = 1),
            actionButton("run_sim", "Run Simulation")
            
        ),

        # Show the results
        mainPanel(
            h1("Results"),
            htmlOutput("results_summary")
        )
    )
)

# Define server logic required to simulate the data
server <- function(input, output, session){
    
    sim_piq_cog <- function(cor_piq_cog = input$rho_piq_cog, 
                            icr_cog = input$icr_cog, 
                            theta_piq = (input$theta_piq_ss - 100) / 15, 
                            br = input$br_cd,
                            cut = input$z_cutoff, 
                            edu = input$edu, 
                            cd_es = input$delta_cd, 
                            cd_theta_cog = (input$theta_piq_ss - 100) / 15 + input$delta_cd, 
                            truncate_cog_lo = (input$truncate_cog_lo - 100) / 15, 
                            truncate_cog_hi = (input$truncate_cog_hi - 100) / 15,
                            sim_n = input$sim_n){
        
        ncd <- as.integer(sim_n * br)
        ncn <- as.integer(sim_n - ncd)
        
        cn_theta_cog <- theta_piq
        cn_theta_iq <- theta_piq
        
        covmat_piq_cog <- matrix(c(1, cor_piq_cog, cor_piq_cog, 
                                   cor_piq_cog, 1, icr_cog,
                                   cor_piq_cog, icr_cog, 1), 
                                 nrow = 3, 
                                 byrow = TRUE)
        
        set.seed(8675309)
        cn <- data.frame(mvrnorm(ncn, mu = c(theta_piq, cn_theta_cog, cn_theta_cog), Sigma = covmat_piq_cog))
        names(cn) <- c("theta_iq_i", "theta_pcog_i", "obs_cog_z")
        cn$edu_i <- edu
        cn$Group <- "CN"
        
        set.seed(90210)
        cd <- data.frame(mvrnorm(ncd, mu = c(theta_piq, cn_theta_cog, cd_theta_cog), Sigma = covmat_piq_cog))
        names(cd) <- c("theta_iq_i", "theta_pcog_i", "obs_cog_z")
        cd$edu_i <- edu
        cd$Group <- "CD"
        
        both <- bind_rows(cn, cd)
        both$Theta <- theta_piq
        both$rho_piq_cog <- cor_piq_cog
        both$cd_br <- br
        both$cut <- cut
        both$delta_cog_cd <- cd_es
        
        ind_sims <- both
        rm(both)
        
        amnart_coef <- structure(list(a1 = c(1.91340390332552, 1.67282788157484, 1.5418612330138, 
                                             2.53618638667661, 1.53016479233213, 1.87969776036209, 0.98432461118343, 
                                             2.14131063453984, 2.70485666635488, 2.83665369994675, 2.35280574705403, 
                                             2.25676781378809, 2.81230449504513, 1.28689002541582, 2.19991500282535, 
                                             2.41651480971578, 0.986398549133714, 3.04225139396231, 1.30167459986889, 
                                             1.67801857916187, 1.51427669207732, 2.7135258993079, 2.50602723853578, 
                                             2.81919094261147, 1.70665115227321, 1.82346149607869, 0.972263976161445, 
                                             2.31160509852141, 2.56380212140471, 2.1781354431772, 2.17858455496791, 
                                             1.39823261798244, 1.72530595846228, 1.0808453375695, 1.7028413877837, 
                                             1.45261440186138, 1.11043429665533, 1.00272654199587, 1.1450902127568, 
                                             2.35620728301628, 1.31869508785511, 1.02204977096453, 2.68930780880669, 
                                             1.30481771746215, 1.05189341606903), 
                                      d = c(3.6581491959349, 2.34510956641479, 
                                            2.62458629121586, 2.9469657293081, 1.74547949250453, 2.23377477346602, 
                                            0.264814499649228, 1.86366081869775, 3.20798630239192, 0.476723166549478, 
                                            3.49181990063508, 3.3175660304417, 2.22694378117984, -0.0996594037305196, 
                                            1.52540974139438, 1.92889757039686, -1.78939406753103, -2.39228208167587, 
                                            -1.39294614739552, -0.831413956936372, -0.575850338683235, 0.00115913024127471, 
                                            -0.374209147025594, 0.236807530162989, -1.06256832530666, 0.504036839298113, 
                                            -0.721459068880437, 1.08454386790892, 1.03742776964994, -1.70855869424773, 
                                            -1.30872899749246, -0.0507183090671781, 0.418586162041944, -1.24482835871387, 
                                            0.150889592410968, -0.60906497517091, -1.63705098949122, -1.33426102762371, 
                                            -0.896194214276525, -3.13372389998924, 1.35837023711015, -1.51665616039459, 
                                            -2.81886568792082, -2.31077448843953, -0.0285736735492737), 
                                      g = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                                      u = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)), 
                                 class = "data.frame", 
                                 row.names = c("Ache", "Debt", "Depot", "Chord", "Bouquet", "Deny", "Capon", "Heir", 
                                               "Aisle", "Subtle", "Nausea", "Gauge", "Naive", "Thyme", "Algae", 
                                               "Fetal", "Quadruped", "Epitome", "Superfluous", "Chamois", "Papyrus", 
                                               "Hiatus", "Simile", "Blatant", "Cellist", "Zealot", "Abstemious", 
                                               "Meringue", "Placebo", "Facade", "Pugilist", "Virulent", "Worsted", 
                                               "Detente", "Sieve", "Chassis", "Beatify", "Scion", "Cabal", "Apropos", 
                                               "Caprice", "Imbroglio", "Hyperbole", "Syncope", "Prelate"))
        set.seed(1234567)
        amnart_sim <- as.data.table(data.frame(simdata(
            a = amnart_coef$a1,
            d = amnart_coef$d,
            N = nrow(ind_sims),
            itemtype = "2PL",
            Theta = matrix(ind_sims$theta_iq_i, ncol = 1))))
        amnart_sim <- setnames(amnart_sim, names(amnart_sim), rownames(amnart_coef))
        amnart_sim[, errors := rowSums(.SD == 0)]
        amnart_sim$edu <- ind_sims$edu
        amnart_sim[, obs_piq := 118.56 - .88 * errors + .56 * edu]
        
        ind_sims <- as.data.table(ind_sims)
        
        ind_sims$obs_piq_ss <- amnart_sim$obs_piq
        ind_sims[, obs_piq_z := (obs_piq_ss - 100) / 15]
        
        ind_sims[obs_cog_z > truncate_cog_hi, obs_cog_z := truncate_cog_hi]  # Truncate distribution like most tests do (e.g., WMS-IV has range of +/- 4 SD)
        ind_sims[obs_cog_z < truncate_cog_lo, obs_cog_z := truncate_cog_lo] # Truncate distribution like most tests do  (e.g., WMS-IV has range of +/- 4 SD)
        ind_sims[, obs_cog_ss  := obs_cog_z * 15 + 100]
        ind_sims[, theta_pcog_ss  := theta_pcog_i * 15 + 100]
        ind_sims[, obs_piq_i  := round(obs_piq_ss, 0)]
        ind_sims[, theta_pcog_ss  := round(theta_pcog_ss, 0)]
        ind_sims[, obs_cog_i  := round(obs_cog_ss, 0)]
        
        ind_sims[, diff_i := obs_cog_i - obs_piq_i]
        ind_sims[, cogchg_ss_i := obs_cog_i - theta_pcog_ss]
        ind_sims[, cogchg_z_i := cogchg_ss_i/15]
        
        
        ind_sims[, below_piq := FALSE][diff_i < cut * 15, below_piq := TRUE][, below_piq := factor(below_piq, levels = c(FALSE, TRUE), labels = c("Negative", "Positive"))]
        ind_sims[, below_fixed_cut := FALSE][obs_cog_i - 100 < cut * 15, below_fixed_cut := TRUE][, below_fixed_cut := factor(below_fixed_cut, levels = c(FALSE, TRUE), labels = c("Negative", "Positive"))]
        ind_sims[, both_agree := 0][below_piq == below_fixed_cut, both_agree := 1]
        ind_sims[, below_both := NA][both_agree == 1 & below_piq == "Negative" & below_fixed_cut == "Negative", below_both := FALSE][both_agree == 1 & below_piq == "Positive" & below_fixed_cut == "Positive", below_both := TRUE]
        ind_sims[, below_both := factor(below_both, levels = c(FALSE, TRUE), labels = c("Negative", "Positive"))]
        ind_sims[, below_piq_only := NA][both_agree == 0 & below_fixed_cut == "Positive", below_piq_only := FALSE][both_agree == 0 & below_piq == "Positive", below_piq_only := TRUE]
        ind_sims[, below_piq_only := factor(below_piq_only, levels = c(FALSE, TRUE), labels = c("Negative", "Positive"))]
        ind_sims[, below_fixed_cut_only := NA][both_agree == 0 & below_piq == "Positive", below_fixed_cut_only := FALSE][both_agree == 0 & below_fixed_cut == "Positive", below_fixed_cut_only := TRUE]
        ind_sims[, below_fixed_cut_only := factor(below_fixed_cut_only, levels = c(FALSE, TRUE), labels = c("Negative", "Positive"))]
        
        ind_sims[, cog_est_norm_i := obs_cog_z - 0]
        ind_sims[, cog_est_piq_i := obs_cog_z - obs_piq_z]
        ind_sims[, cog_resid_norm_i := cog_est_norm_i - cogchg_z_i]
        ind_sims[, cog_resid_piq_i := cog_est_piq_i - cogchg_z_i]
        
        sim_summary <- ind_sims %>% 
            group_by(rho_piq_cog) %>%
            summarise(tp_piq = sum(Group == "CD" & below_piq_only == "Positive", na.rm = TRUE),
                      fp_piq = sum(Group == "CN" & below_piq_only == "Positive", na.rm = TRUE),
                      fn_piq = sum(Group == "CD" & below_piq_only == "Negative", na.rm = TRUE),
                      tn_piq = sum(Group == "CN" & below_piq_only == "Negative", na.rm = TRUE),
                      odds_cd_piq = tp_piq / fn_piq,
                      odds_cd_nm = fp_piq / tn_piq,
                      or_piq = odds_cd_piq / odds_cd_nm,
                      log_or_piq = log(or_piq),
                      se_log_or = sqrt(1 / tp_piq + 1 / fp_piq + 1 / fn_piq + 1 / tn_piq),
                      ci95_l = exp(log_or_piq - qnorm(.975) * se_log_or),
                      ci95_u = exp(log_or_piq + qnorm(.975) * se_log_or))
        
        out <- sim_summary
        return(out)
    }

    output$results_summary <- eventReactive(input$run_sim, {
        withProgress(message = "Running simulation...", value = 0, {
            sim_summary <- sim_piq_cog()
            
            out_text <- paste0("Among the ", format(input$sim_n, big.mark = ",", scientific = FALSE), 
                               " cases simulated, there were ", 
                           format(rowSums(sim_summary[c("tp_piq", "fp_piq", "fn_piq", "tn_piq")], na.rm = TRUE), 
                                  big.mark = ",", 
                                  scientific = FALSE),
                           " (", 
                           100 * round(rowSums(sim_summary[c("tp_piq", "fp_piq", "fn_piq", "tn_piq")], 
                                             na.rm = TRUE) / input$sim_n, 3) , 
                           "%) disagreements. These are shown in the table above. ",
                           "Given the inputs to this simulation, when a disagreement does occur, 
                           the ratio of the odds in favor of the pIQ-based comparison being correct, 
                           relative to the odds in favor of the norm-based comparison being correct, is OR = ", 
                          round(sim_summary$or_piq, 2),
                   ", 95% CI [", 
                   round(sim_summary$ci95_l, 2), 
                   ", ",
                   round(sim_summary$ci95_u, 2),
                   "].")
            
            out_table <- data.frame(Cognitively_Impaired_Population = c(sim_summary$tp_piq, sim_summary$fn_piq),
                               Cognitively_Normal_Population = c(sim_summary$fp_piq, sim_summary$tn_piq),
                               Total = c(sim_summary$tp_piq + sim_summary$fp_piq, sim_summary$fn_piq + sim_summary$tn_piq))
            rownames(out_table) <- c("pIQ Abnormal / norm-based Normal",
                                "pIQ Normal / norm-based Abnormal")

            out <- print(xtable(out_table, caption = out_text, align = c("l", "c", "c", "c")), type = "html")
            return(out)
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
