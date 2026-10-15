# CLAUDE.md — Concentration & Peer Effects in Youth Crime

## Research question

> Does younger students' exposure to older students when the school bell rings increase youth crime?

Two channels that the paper should try to separate:

- **Peer effect**: *who* is released with you. 9th graders (EF) leaving at the same time as high-school (EM) students get exposure to older peers.
- **Concentration effect**: *how many* young people are on the street at the same time and place, whatever their age.

The paper is at an early stage. Current work: **choosing the source of variation** and **mapping the confounders**, first for the question itself and then for each identification strategy. Help with that thinking. Don't jump to estimation code before the design is settled.

## Project layout

- [misc/main.tex](misc/main.tex): working notes (question, sample, candidate designs, confounders). This is the design document. Its text is changed only when the user writes "CHANGE TEXT" (see Conventions).
- `code/`: scripts only, numbered in run order. `01` must run first (every other script reads `amostra_id_escola.rds`); `09` → `10` → `11`/`12` depend on each other; the rest are independent.
  - [01_amostra.R](code/01_amostra.R): defines the **old RMBH sample** (2015 cross-section). Saves `amostra_id_escola.rds`. To be replaced by all MG state schools (see Sample).
  - [02_mudancas_saida_9ano.R](code/02_mudancas_saida_9ano.R): 2007–2017 panel; detects within-school changes in the set of 9th-grade release times. Ends in an unfinished line (`sample<-`), so it doesn't run as a whole.
  - [03_saida_compartilhada_9ano_em.R](code/03_saida_compartilhada_9ano_em.R): school-year indicator "no 9th-grade release time equals any EM release time"; finds schools that switch from not shared to shared.
  - [04_desenho_a_rct.R](code/04_desenho_a_rct.R): counts eligible cells for design (a) → `rct_elegibilidade.xlsx`.
  - [05_desenho_b_twfe.R](code/05_desenho_b_twfe.R): switchers for design (b) → `twfe_viabilidade.xlsx`.
  - [06_horarios_1em_manha.R](code/06_horarios_1em_manha.R), [07_horarios_9ano_manha.R](code/07_horarios_9ano_manha.R): most frequent morning release times by year, 1st-year EM and 9th grade.
  - [08_horarios_1em_2em.R](code/08_horarios_1em_2em.R): most frequent release times of 1st-, 2nd- and 3rd-year EM, 2007–2017, all MG state schools (the current sample). Six line graphs, one per grade × shift (morning, afternoon) → `horarios_em_series_{manha,tarde}_{1,2,3}ano.png`, shown in `misc/main.tex`.
  - [09_painel_horarios_1em.R](code/09_painel_horarios_1em.R): school panel 2011–2014 of 1st-year EM morning release and afternoon start → `painel_*.rds`.
  - [10_mudancas_horario_1em.R](code/10_mudancas_horario_1em.R): schools that delayed 1st-year EM release by ≥50 min before 2014 → `id_mudou_50_*.rds`, `tratados_saida_1em_manha.png`.
  - [11_horarios_1em_2em_sem_antecipadas.R](code/11_horarios_1em_2em_sem_antecipadas.R): `08` restricted to the sample, without those early schools.
  - [12_hist_diff_saida_1em.R](code/12_hist_diff_saida_1em.R): per-school difference between 1st-year and 2nd/3rd-year EM release time, 2013–2015.
  - [0_escolas_reinventando.R](code/0_escolas_reinventando.R): INEP codes of the Reinventando o Ensino Médio pilot schools: 11 in 2012 (SRE Metropolitana C, Belo Horizonte; `reinventando_em/escolas_2012.png`) and 122 in 2013. Saves the 133 together, as one vector, to `output/id_escolas_reinventando.rds`.
  - [13_tabela_escolas_2013.R](code/13_tabela_escolas_2013.R): descriptive table of MG state schools with EM in 2013, sample = without the pilot schools → `tabela_escolas_2013.xlsx`.
- `output/`: everything the scripts produce (`.rds`, `.xlsx`, `.png`). The .tex reads figures from here (`\graphicspath{{../output/}}`).
- `reinventando_em/`: source documents on Reinventando o Ensino Médio. `misc/main_papers/`: reference papers.

## Data

- **Censo Escolar (INEP)** via `basedosdados` (BigQuery; `set_billing_id("humancapitalindex")`), table `br_inep_censo_escolar.turma`. Fields used: `hora_inicial`, `minuto_inicial`, `numero_duracao_turma`, `etapa_ensino`, `quantidade_matriculas`, `rede`.
  - Release time = `hora_inicial*60 + minuto_inicial + numero_duracao_turma` (minutes after midnight). This assumes `numero_duracao_turma` is **daily minutes**. Verify against the Censo dictionary.
  - `etapa_ensino`: `11` = 8ª série (EF 8 anos), `41` = 9º ano (EF 9 anos) → coded `grade = 9`. `25`, `30`, `35` = **1st year** of EM (regular / integrado / normal-magistério) → coded `grade = 10`. **2nd and 3rd year EM are not included yet** (e.g. 26/27, 31/32, 36/37). If "older students" means all of EM, add them.
- **Educational data for MG, 2007–2017, including proficiency scores** (SIMAVE/PROEB). Note which identifiers link it to Censo.
- **Crime / violence outcome: source not yet defined.** This is the main open data question (see below).

## Sample

- **All state schools (rede estadual) in Minas Gerais.** Decided on 2026-10-13; this replaces the RMBH sample below, which is no longer used.
- Municipal schools are excluded because they generally don't offer EM, so there are no older peers in the building.
- Unit of analysis (current plan): class (turma).
- Define the sample year by year. A sample fixed on one year and applied to 2007–2017 drops or mis-classifies schools that open, close, or gain or lose EM.
- **Code not yet updated:** `01_amostra.R` still builds the RMBH sample (`amostra_id_escola.rds`), and scripts 02–07 and 09–12 filter on it. Only `08_horarios_1em_2em.R` already runs on all MG state schools. All results recorded below (Open question 4, figures in `output/`) come from the RMBH sample and need to be redone.
- Old sample (dropped, kept for reference): Metropolitan Region of Belo Horizonte (34 municipalities), state schools that offer both 9th grade and EM. 2015: 283 schools, ~28k 9th-grade students, 857 classes. State schools = 57% of public 9th-grade enrollment in the RMBH; state schools that also have EM = 44.4%.

## Current design (2026-10-15)

- Students: 2nd- and 3rd-year EM. Variation: the 2014 one-year change in 1st-year EM schedules (1st year leaves ~50 min later; returns in 2015).
- **Decision (2026-10-15): assume all schools implemented REM in 2014; no control schools. Main design = time windows around the bell** (geocoded, time-stamped crime: release window vs other windows, school days vs non-school days, 2014 vs 2013/2015). Treated schools still defined by (i)–(iii); Table 2 of `main.tex` (`output/tabela_tratamento.tex`) now shows only the criteria and treated (598). The treated-vs-control DiD below is no longer the main design.
- Treated / control are defined in `misc/main.tex` ("Treated and control schools") and counted by `code/1_tabela_escolas_2013.R` → `output/tabela_tratamento.tex`. **Analysis sample (decided 2026-10-15), Table 1 of `code/1_tabela_escolas_2013.R`, sequential exclusions in 2013: (1) state schools with 1st-, 2nd- and 3rd-year EM classes, any shift (2,174) − (2) pilot schools (132) − (3) schools with afternoon EM classes (547) − (4) schools with different release times within a grade in the morning (95) − (5) schools without morning EM classes, i.e. night only (197) = 1,203 schools.** Script 1 saves it to `output/id_escolas_amostra.rds`, which `2_grafico_linha_horario.R` (morning graphs only) and `3_hist_diff.R` read, so run 0 → 1 → 2, 3. Night classes are allowed. Criteria use only morning classes, in schools with the 1st year and at least one of the 2nd/3rd years in the morning in 2013 and 2014 (1,125 schools); the 1st year is compared with whichever of the 2nd/3rd years the school has: treated = (i) shared release in 2013 (≤ 5 min) + (ii) 1st year leaves 40+ min later in 2014 with 1st-year duration up 40+ min + (iii) 2nd/3rd-year start and duration unchanged (598 schools); control = (i) + (iii) + still shared in 2014 (229 schools). Don't read "only morning" as excluding night classes.
- Control schools (`code/4_estudo_controles.R` → `output/estudo_controles.xlsx`): REM was universalized to all ~2,189 state HS schools in 2014, 1st year only, so controls are schools that *declared* no change. 98% report exactly the same 1st-year duration in 2013 and 2014 (260/265/270 min); none had 6 periods (300+ min) in 2013 and none moved the start time earlier. Controls are smaller (7.1 vs 9.4 HS classes; 215 vs 309 students), more rural (7.9% vs 2.3%), less often in BH (1.7% vs 7.7%), spread over 169 municipalities. Stale-reporting test (part C of script 4): outside 2013–2015, controls change their declared morning schedule as often as treated schools (31–38% of schools per year in 2009–2012, 21–25% in 2015–2017; 29% never change in both groups), so they don't look like schools that copy last year's Censo. A one-off failure to report the 6th period in 2014 can't be ruled out; next check: Censo docente (employability-area teachers/disciplines in their 1st-year classes) or SEE administrative data. If all schools were in fact treated, the treated-vs-control DiD goes to zero; designs that survive: reversal within school (2014 vs 2013/2015), intensity among treated, night shift as within-school control (night had no 6th period, Res. 2.486 art. 7 §3; but art. 11 restricted night enrollment in 2014), and time windows around the bell. Resolution SEE 2486/2013 (in `misc/notas.txt`) not yet found.
- The 9th-grade designs below are the earlier setup, kept for reference.

## Candidate sources of variation

(Full notation is in the .tex.)

| | Level | Comparison | Main advantage | Main threats |
|---|---|---|---|---|
| (a) | Class, within school × shift × year ("RCT" within school) | Cells with ≥2 9th-grade classes, at least one released with EM and at least one not; difference in means inside the cell | Holds school, neighborhood, shift, and year fixed; simple to explain | Needs a class/student-level outcome; release time is tied to class duration and start time; non-random class assignment; only cells with within-cell variation contribute |
| (b) | School × year | Schools that switch shared → not shared release vs not-yet-treated | Within-school, absorbs fixed school traits | Few switchers; switches may be reporting noise; policy endogeneity (school changes schedule *because of* violence); switches coincide with enrollment/class-count changes; staggered TWFE bias |
| (c) | School × shift | Shared vs not shared, morning vs afternoon | Uses shift variation | Shift sorting of students; time of day differs by shift |
| (d) | Student, 8th → 9th | Students whose shared status changes vs those whose status doesn't | Individual FE | Needs a longitudinal student ID and an individual outcome; age-crime profile rises steeply 14→15; school switchers |

**Current choice:** (a), framed as an "RCT" inside each school × shift × year × class-duration cell, not as a dif-dif. Treated and control classes must have the same duration, so time in school is held fixed (`code/04_desenho_a_rct.R` counts eligible cells this way). The identifying assumption is that, inside a cell, which class gets the shared release time is as good as random. It needs (i) a balance test inside cells (age-grade distortion, repetition, prior scores, sex, class size), and (ii) a check on start time: with equal duration, a different release time is a different start time. (b) is a possible complement or event study but depends on switchers being real and exogenous; there, use heterogeneity-robust DiD (de Chaisemartin–D'Haultfœuille handles treatment switching on/off; Callaway–Sant'Anna / Sun–Abraham for absorbing treatment) rather than plain TWFE.

A design not yet in the notes: **(e) time × place variation in crime**. With geocoded, time-stamped police records, compare youth crime near a school in the minutes after release on days/years when 9th and EM release together vs apart. That design speaks to *concentration* directly.

## Confounders

### A. Of the research question (any design)

1. **Sorting of students into schedules/classes.** Schools allocate students to shift and class by age, repetition, behavior, work status, or ability. Overage 9th graders (distorção idade-série) are both "older" and higher-risk. Separate *exposure to older peers outside class* from *having older classmates*.
2. **Time of day.** Shared release is correlated with clock time (e.g. 12:20 vs 11:30; afternoon vs evening). Clock time changes parental supervision (work hours), daylight, policing shifts, and street activity. Exclude or separate the **noturno** shift.
3. **Unsupervised time / incapacitation.** A different release time usually means a different start time or class length. Fewer hours in school means more free time. This is a separate channel (Jacob & Lefgren 2003-type incapacitation) and it confounds shared vs not shared.
4. **School size and density.** Schools that release everyone together are larger and in denser, often more violent, areas (already in notes).
5. **Neighborhood crime and nearby schools.** Other schools releasing at the same time nearby add to concentration and violate SUTVA (spillovers across schools). Controls may be treated too.
6. **Reverse causality at the school level.** Principals stagger dismissal *in response to* fights at the gate. Treatment then responds to past violence (Ashenfelter-dip / anticipation).
7. **Infrastructure constraints.** Staggered times often come from lack of rooms (intermediate shifts, overcrowding). These also correlate with school quality.

### B. Of each identification strategy

- (a) Duration is fixed inside the cell, so within-cell differences in release time are differences in start time (clock time, see A.2); hours in school (A.3) no longer differ. The school may assign "problem" classes to specific times. With few classes per school-shift-duration, identifying variation may be thin; count how many cells have within-cell variation.
- (b) Changes in release time that attract different 9th-grade students (in notes). Simultaneous changes: number of classes, EM expansion/closure, MG network reorganization, full-time programs (e.g. Reinventando o Ensino Médio, Educação Integral), EF8 → EF9 transition. Changes in Censo-reported times that are data-entry noise and don't reflect real schedule changes (check persistence, minute-level jumps).
- (c) Selection into morning vs afternoon differs systematically (working students, older students in afternoon/evening). The shift × shared interaction still requires parallel differences across shifts.
- (d) Grade progression is itself the change in age. Status changes correlate with moving schools, repeating, or changing shift. Attrition (dropout) correlates with crime.

### C. Measurement

- Exact-minute matching of release times is strict. Test windows (±5/10/15 min) and use a continuous measure: the number of EM students released within X minutes (this intensity measure also separates concentration from the pure binary).
- "Same shift" restriction: shared release must be defined within the same shift (TODO noted in code).
- Censo times are declared schedules, not actual dismissal.

## Open questions to resolve

1. **Outcome data**: police records (REDS/SEDS-MG, geocoded, with offender age and time?), juvenile justice (atos infracionais), or school-reported violence (SAEB/Prova Brasil director and student questionnaires)? The answer determines whether (a)/(d) are feasible.
2. Is there a stable student ID across years (Censo matrícula / SIMAVE) for design (d)?
3. How to separate peer vs concentration effects: compare exposure to *older* students (EM) vs exposure to the *same number* of same-age students (other 9th-grade or 8th-grade classes).
4. How many school × shift × year cells have within-cell variation for (a)? How many real switchers exist for (b) after cleaning noise? (The answers below use the old RMBH sample; redo them on all MG state schools.)
   - Answer for (a), from `code/04_desenho_a_rct.R` (2009–2017, 283 schools): with same class duration, 5 cells in 4 schools; without it, 56 of 2,876 cells in 52 schools. Windows of ±5/10/15 min do not help (5 cells with same duration; 38–40 without). Release-time gaps are bimodal: 55% of 9th-grade classes leave at the exact EM minute, 43% leave more than 30 min away, 1.5% in between. Design (a) has almost no sample as specified.
   - Answer for (b), from `code/05_desenho_b_twfe.R` (school × year, 2009–2017, 2,520 school-years): 262 shared → zero events in 214 schools, but 142 are in 2014, when EM class duration rises about 50 min for that year only (84% of schools have zero shared release in 2014; 169 revert in 2015). Probably the Reinventando o Ensino Médio sixth period; confirm. The 2014 jump is in **1st-year EM only**: 2nd-year EM morning classes (etapas 26/31/36) keep leaving at 11:20–11:30 in 2014 (`code/08_horarios_1em_2em.R`), as does 9th grade (`code/07_horarios_9ano_manha.R`). The afternoon shift shows the same pattern: 1st-year EM moves from 17:00–17:30 to 17:50–18:20 in 2014 only, 2nd-year EM stays at 17:00–17:30. So the 2014 loss of shared release is an artifact of defining "older students" as 1st-year EM. Dropping 2014: 140 events in 119 schools (16–25 per year, 150–190 not-yet-treated controls per year), 57 stay at zero to 2017, 49 last one year, 50 schools have ≥2 years shared before and ≥2 years zero after. Treatment is not absorbing (95 schools switch more than once), so use de Chaisemartin–D'Haultfœuille.
   - Restriction for (b): the 9th-grade shift (set of shifts with 9th-grade classes) cannot change between t-1 and t; the EM shift can. With it and without 2014: 63 events in 60 schools (2–15 per year, 110–176 controls per year), 15 stay at zero to 2017, 30 last one year, 16 clean. In 31 of the 63 the EM shift changes; in 15 the nearest EM release is within 15 min. Requiring the 9th-grade shift to be fixed over the whole period leaves 137 schools and 36 events (11 clean).

## Conventions

- R + tidyverse; data pulled with `basedosdados::read_sql`. Code comments are in Portuguese. Keep that style when editing existing scripts.
- Scripts read and write everything in `output/` through `dir_output <- if (dir.exists("../output")) "../output" else "output"`, so they run from `code/` or from the project root. New scripts: same header, next number, outputs only via `file.path(dir_output, ...)`.
- **Do not create or change the user's text in `misc/main.tex` unless the request contains the words "CHANGE TEXT".** This covers paragraphs, bullets, headings, captions and notes. Without those words, leave the text as it is. If a task seems to need a text change (for example, a number or a figure the text cites no longer matches the output), say so in the reply instead of editing.
- "COMPILE AND EXPORT": use the project skill `compile-export` (`.claude/skills/compile-export/`), which compiles `misc/main.tex` and copies `main.pdf` to `C:\Users\giova\iCloudDrive\PhD\Thesis`.
- The design notes are in LaTeX (`misc/main.tex`). When the design changes, update this file; the .tex only under the rule above.
- When discussing identification, always state the comparison, the unit, the fixed effects, and the identifying assumption explicitly, and list what would violate it.
