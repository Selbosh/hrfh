

# Health Research from Home (HRfH)

Synthetic data for hackathon.

## Parameters

- Ordinal scale from 0–10
- Assume it is ‘pain’ but could be any symptom ot outcome (label it
  ‘pain’ for now)
- Follow-up for 6 months
- Assume in first iteration that people report every day
- Groups of people
  - High disease severity
  - Moderate disease severity
  - Low disease severity
  - …where ‘severity’ means the average level of pain
- But people will *transition* between these levels of disease activity
- Assume that the transition probabilities are known
  - Usually in response to treatment
  - If high or medium severity, clinical treatment will want to get that
    under control
  - Change in treatments 6 weeks to 3 months for ‘high’
  - Then can have a rule that treatment pushes (e.g. 2/3 of) people from
    high to moderate/low state
- Certain people are more likely to go back to high disease severity
- **Treatment is modelled in the background but not recorded explicitly
  in the data**
- Some patients more likely to respond to serial treatments, others not
  (see 2/3 above)

### Flares

- If latent continuous variable increases by a certain amount in a
  certain window, then there is a high probability (not a guarantee) of
  recording an onset flare event – which is a discrete event.
- Duration of flare event – how long does it last for?
- Do they return to where they were at the end of the flare or is it a
  changepoint.
- Some/all flares are not predictable in advance. So flares could be a
  point process that changes the latent trajectory, rather than the
  other way round.
- But participants may try to approach the problem of trying to predict
  from the ‘pre-flare period’.
  - So flare can induce sometimes but not always a pre-flare ramping up
    period. Either linear ramping up in latent variable.
  - Or increase in variability/amplitude? Or both.
- Flares are ~~self-exciting process~~ – patients continually have
  flares until they are treated. Or just the hazard rate of flare
  increases with latent state of disease activity.
- Low disease activity: 4 or 5
- Definition of a flare is 3 or more units, a sudden increase and
  sustained for a fixed minimum duration.
- Challenge is not to find the flares but just not to miss them with
  their algorithm.
- Model as a directed acyclic graph
- Write up as a paper(?) or parametrizable simulator to provide e.g. MSc
  students access to data. With Shiny interface.
- Shoei is looking after GitHub page with resources. So can share as web
  app or R package.

``` r
library(ggdag)
library(ggplot2)

dag <- dagify(
  Latent_Pain_Disease_Activity ~ Medication_Adherence + Physical_Activity + Sleep_Quality + Weather_Conditions + Stress_Levels + Dietary_Intake + Joint_Swelling + Fatigue,
  Daily_Pain_Score ~ Latent_Pain_Disease_Activity,
  Flare_Occurrence ~ Latent_Pain_Disease_Activity + Weather_Conditions + Stress_Levels + Dietary_Intake + Joint_Swelling + Fatigue,
  exposure = "Medication_Adherence",
  latent = "Latent_Pain_Disease_Activity",
  outcome = "Daily_Pain_Score"
)

dag %>%
  tidy_dagitty() %>%
  dplyr::mutate(colour = ifelse(name == 'Daily_Pain_Score', 'Colour 1', 'Colour 2')) %>%
  ggplot() +
  aes(x, y, xend = xend, yend = yend) +
  geom_dag_point(aes(colour = colour)) +
  geom_dag_edges() +
  geom_dag_text() +
  theme_dag_grey() +
  theme(legend.position = 'none')
```

![](README_files/figure-commonmark/packages-1.png)

See
<https://cran.r-project.org/web/packages/simDAG/vignettes/v_sim_from_dag.html>
for more information.
