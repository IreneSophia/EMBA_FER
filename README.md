# EMBA: using the Bayesian Brain to investigate the specificity of emotion recognition differences in autism and ADHD

Mutual social interactions require people to be aware of the affective state of their counterpart. An important source for this is facial expressions, which can be indicative of the emotions experienced by the other person. Individuals with autism spectrum disorder (ASD) often have difficulties with this function. Despite the extensive documentation of such differences, it is still unclear which processes underlie attenuated emotion recognition in ASD. In this project, we aim to use a prominent human brain theory called the Bayesian Brain to evaluate the impact of three mechanisms on emotion recognition in individuals with ASD at the neural and behavioural levels: (1) emotional face processing, (2) learning of associations between contextual cues and facial expressions associated with emotions, and (3) biased attention for faces. We also plan to include individuals with attention deficit hyperactivity disorder (ADHD) as clinical controls in addition to a sample of people with no neurodevelopmental or psychiatric disorder (comparison group, COMP). This allows us to determine whether differences in emotion recognition can be attributed to attentional deficits or are unspecific for the included developmental disorders. The results of this project will not only shed more light on the causes of deficits in emotion recognition in people with ASD, but also provide the basis for developing a model of the similarities and differences in processes of the Bayesian Brain in neurodevelopmental disorders.

## Facial emotion recognition (FER)

In this repository, we will focus on the paradigm measuring facial emotion recognition (FER) compared to facial species recognition (FSR). For both tasks, we have created 5 s long videos by morphing an emotionally neutral face with a target face. In the case of the FER task, this target face is the same person expressing one of four emotions (anger, fear, happiness, sadness), while in the FSR task the target face belongs to one of four species (apes, cats, dogs, lions). These videos show an emotionally neutral human face gradually and continuously changing into a human face expressing an emotion (FER) or into an emotionally neutral face of another species (FSR). The participants’ task is to stop the video as soon as they have recognised the emotion or species. Then, they have to choose the correct emotion or species out of the four options. They are not presented with the final frame, therefore, the amount of information they have for their decision depends on when they stop the video. The remaining part of the video that participants did not need to see to correctly recognise the emotion or species captures their discrimination sensitivity. Using the FSR task in addition to the FER task allows us to determine whether differences between groups are due to facial emotion recognition specifically . We will compare three groups (ADHD vs. ASD vs. COMP) and use an eye tracker to measure gaze patterns. This task has been preregistered on [OSF](https://osf.io/jxacw).

Participants also perform three additional paradigms: a dot-probe task to measure face attention bias (FAB), a probabilistic learning paradigm (PAL) and a visual mismatch task (VMM). The preregistrations for this project are on OSF and currently embargoed. The preregistrations will be made public when manuscripts are submitted. 

This repository is a work in progress. The script are continuously augmented.

## How to run this analysis

This repository includes scripts for the evaluation of the stimuli, presentation of the paradigm, preprocessing of the data and analysis. Due to privacy issues, we only share preprocessed and anonymised data. Therefore, only the following analysis RMarkdown scripts can actually be run based on this repository: 

* `brms-analyses_FER.Rmd` : behavioural analysis, exploratory and hypothesis testing
* `brms-analyses_FER-ET-H.Rmd` : eye tracking analysis, hypothesis testing
* `brms-analyses_FER-ET-E.Rmd` : eye tracking analysis, exploratory

These scripts also use scripts from the `helpers` folder. There are some absolute paths in these scripts within if statements. Downloading everything in this repository should ensure that these are not executed. 

We also share the models and the results of the simulation-based calibration. **Rerunning these, especially the SBC, can take days depending on the specific model.** Runtime of the scripts using the models and SBC shared in this repository should only take a couple of minutes. The scripts will create all relevant output that was used in the manuscript. If you need access to other data associated with this project or want to use the stimuli / paradigm, please contact the project lead (Irene Sophia Plank, 10planki@gmail.com). 

The `experiment` folder contains the scripts needed to present the experiment as well as the RMarkdown containing all information regarding the stimulus evaluation and selection. 

The `prepro` folder contains scripts used during preprocessing. All scripts contain information in the header regarding their use. 

### Versions and installation

Each html file contains an output of the versions used to run that particular script. It is important to install all packages mentioned in the html file before running a specific analysis file. Not all packages can be installed with `install.packages`, please consult the respective installation pages of the packages for more information. If the models are rerun, ensure a valid cmdstanr installation. 

For preprocessing of the eye tracking data, MATLAB R2023a was used. 

## Variables

Data is shared in RDS and RData files which can be read into R. In the following, you can find a description of the columns contained in the data frame saved in these files. 

### `FER_data.RData`

This file contains the following data frames: 

`df.fer` and `df.exp`

* subID : anonymised participant ID
* diagnosis: diagnostic status of this participant, either ADHD, ASD, BOTH or COMP (comparison group, no psychiatric diagnoses)
* trl : trial number (1 to 64)
* video : KDEF ID and emotion of the video
* frames : how many frames the participant viewed (max. 300)
* emo : emotion portrayed in the stimulus (AF = afraid, AN = angry, HA = happy, SA = sadness)
* opt : option chosen by the participant (AF = afraid, AN = angry, HA = happy, SA = sadness)
* disc : discrimination threshold, NA if answered incorrectly, otherwise percentage of video watched
* acc : whether the emotion was chosen correctly, i.e., opt == emo, either TRUE or FALSE
* acc.code : same, but with 0 for FALSE and 1 for TRUE
* fsr.disc : facial species recognition discrimination threshold of this participant, estimated from the control task
* fsr.acc : overall accuracy of this participant in the control task

`df.table`

* measurement : questionnaire or socio-demographic variable
* ADHD : mean and standard errors or counts for the gender identities for the ADHD group
* ASD : mean and standard errors or counts for the gender identities for the ASD group
* BOTH : mean and standard errors or counts for the gender identities for the ADHD+ASD group
* COMP : mean and standard errors or counts for the gender identities for the COMP group
* logBF10 : logarithmic Bayes Factor comparing the model including diagnosis to the null model

`df.fix`

* subID : anonymised participant ID
* trl : trial number (1 to 64)
* AOI : in which area of interest was this fixation (forehead, eyes, mouth, nose)
* fix.total : total duration of fixations regardless of AOI in this trial
* n.fix : number of fixations in this trial on this AOI
* fix.dur : fixation duration to this AOI in this trial
* fix.perc : fixation duration to this AOI in this trial relative to the total duration of fixations in this trial
* emo : emotion portrayed in the stimulus (AF = afraid, AN = angry, HA = happy, SA = sadness)
* video : KDEF ID and emotion of the video
* frames : how many frames the participant viewed (max. 300)
* opt : option chosen by the participant (AF = afraid, AN = angry, HA = happy, SA = sadness)
* disc : discrimination threshold, NA if answered incorrectly, otherwise percentage of video watched
* acc : whether the emotion was chosen correctly, i.e., opt == emo, either TRUE or FALSE
* n.fix.total : total number of fixations of this participant over the course of the paradigm
* diagnosis: diagnostic status of this participant, either ADHD, ASD, BOTH or COMP (comparison group, no psychiatric diagnoses)

`df.sac`

* subID : anonymised participant ID
* trl : trial number (1 to 64)
* AOI : from which area of interest to which other AOI was this saccade (e.g., "eyes fore" means from the eye region to the forehead)
* n.sac : predicted number of saccades had the participant viewed the full video
* emo : emotion portrayed in the stimulus (AF = afraid, AN = angry, HA = happy, SA = sadness)
* video : KDEF ID and emotion of the video
* frames : how many frames the participant viewed (max. 300)
* opt : option chosen by the participant (AF = afraid, AN = angry, HA = happy, SA = sadness)
* disc : discrimination threshold, NA if answered incorrectly, otherwise percentage of video watched
* acc : whether the emotion was chosen correctly, i.e., opt == emo, either TRUE or FALSE
* n.sac.total : total number of predicted saccades of this participant over the course of the paradigm
* diagnosis: diagnostic status of this participant, either ADHD, ASD, BOTH or COMP (comparison group, no psychiatric diagnoses)

`df_first`

* subID : anonymised participant ID
* trl : trial number (1 to 64)
* pic_start : frame at which the first fixation started
* pic_end : frame at which the first fixation ended
* AOI : in which area of interest was the first fixation (forehead, eyes, mouth, nose)
* emo : emotion portrayed in the stimulus (AF = afraid, AN = angry, HA = happy, SA = sadness)
* video : KDEF ID and emotion of the video
* frames : how many frames the participant viewed (max. 300)
* opt : option chosen by the participant (AF = afraid, AN = angry, HA = happy, SA = sadness)
* disc : discrimination threshold, NA if answered incorrectly, otherwise percentage of video watched
* acc : whether the emotion was chosen correctly, i.e., opt == emo, either TRUE or FALSE
* n.first : how many first fixations within 60 frames did this participant have in total
* diagnosis: diagnostic status of this participant, either ADHD, ASD or COMP (comparison group, no psychiatric diagnoses)
* AOI.code : AOI but coded as numbers

`df_last`

* subID : anonymised participant ID
* video : KDEF ID and emotion of the video
* diagnosis: diagnostic status of this participant, either ADHD, ASD or COMP (comparison group, no psychiatric diagnoses)
* emo : emotion portrayed in the stimulus (AF = afraid, AN = angry, HA = happy, SA = sadness)
* AOI : in which area of interest was the last fixation before stopping the video (forehead, eyes, mouth, nose)
* AOI.code : AOI but coded as numbers

as well as `df.exc` (group and number of excluded participants), `df.sht` (outcome of shapiro test for the demographic and questionnaire values), `gen.desc` (gender descriptions not on the binary male/female), `tb.gen` (table with gender, trans/cis and diagnostic group) and the results of the contingency tables (`ct.full` and `ct.mf` for only male and female participants).

## Result files

All brms results are saved in RDS files which can be read into R. The following files are shared: 

`m_acc.rds`: bernoulli model assessing the influence of diagnostic status and emotion as well as their interaction on accuracy. 

`m_com.rds`: truncated gaussian model assessing the influence of diagnostic status, emotion and their interaction as well as discrimination threshold in the control task on emotion discrimination threshold. 

`m_fer.rds`: truncated gaussian model assessing the influence of diagnostic status, emotion and their interaction on emotion discrimination threshold. 

`m_first.rds`: categorical model assessing the influence of diagnostic status on the area of interest of the first fixation. 

`m_fix.rds`: hurdle lognormal model assessing the influence of diagnostic status, emotion, AOI and their interactions on dwell times in percent of total fixation duration. 

`m_last.rds`: categorical model assessing the influence of diagnostic status, emotion and their interaction on the area of interest of the last fixation before the video was stopped. 

`m_sac.rds`: gaussian model assessing the influence of diagnostic status, emotion and their interaction on predicted number of saccades if participants had watched the full video. 

## Project members

* Project lead: Irene Sophia Plank
* NEVIA lab PI: Christine M. Falter-Wagner
* Project members (alphabetically): Krasniqi, Kaltrina; Nowak, Julia; Pior, Alexandra; Yurova, Anna

## Licensing

GNU GENERAL PUBLIC LICENSE
