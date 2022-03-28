---
title: "Scotland’s Alcohol and Drug Workforce: Bridging narrative"

output: 
 word_document:
  reference_docx: output_test.docx 
---

```{r setup, include=FALSE}

library(tidyverse)
library(flextable)
# set chunks defaults
knitr::opts_chunk$set(
 echo    = FALSE,
 message  = FALSE,
 warning  = FALSE, 
 dpi = 300
)

theme <- 
 theme_bw()+
 theme(axis.text.x = element_text(size=10),
    axis.text.y = element_text(size=10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size=16),
    strip.text.x = element_text(size =11),
    legend.position = 'bottom',
    legend.text=element_text(size=10),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.x=element_blank(),
    panel.grid.minor.y = element_blank())
```

## 1. Introduction

Scottish Government announced a National Mission on drug-related deaths^[[Scottish Government, 2021a](https://www.gov.scot/policies/alcohol-and-drugs/national-mission/)] in January 2021 to address Scotland’s record numbers of drug fatalities. Similarly Scotland’s alcohol death rates are also consistently higher than those of England and Wales, as well as the rest of Europe^[[National Records of Scotland, 2021](https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/deaths/alcohol-deaths)]. In addressing these challenges the Scottish Government is committed to building a resilient and skilled workforce in the drug and alcohol treatment sector.

To support this work, Health and Social Care Analysis have undertaken a programme of research to better understand the drug and alcohol workforce. This programme has entailed both synthesising existing datasets and generating new data, which has resulted in the production of the following series of documents:

  * A rapid evidence review of the literature around the workforce in drug and alcohol services in Scotland and elsewhere;
  * A review of skills and qualification offerings relevant to this workforce, both in higher and further education settings as well as continuing professional development (CPD)/upskilling opportunities for those already working in the sector;
  * A survey of all frontline services currently delivering on behalf of Alcohol and Drug Partnerships (ADPs), run between 20 November and 17 December 2021;
  * A series of reference groups with select survey respondents, undertaken in March 2022, to gather further qualitative data.

The purpose of this paper is to summarise the main findings from each of these workstreams, categorised under three key themes: recruitment, retention and service design. It will also present considerations for how the qualitative and quantitative evidence arising from this research might be used to inform policy development as well as service design going forward. This summary will be accompanied by reports for each of the aforementioned workstreams which explore the issues here in full.

## 2. Recruitment

The data showed that recruitment was a major issue for the drugs and alcohol workforce. Evidence from the survey of services showed a sector-wide vacancy rate^[NES's definition of vacancy rate was employed for this research. For more details, see [NES, 2022](https://turasdata.nes.nhs.scot/about-our-data-and-reports/data-sources-and-quality-assurance/vacancy-surveys/)] of 8.8%. This is higher than vacancy rates amongst allied health and medical professions overall at the nearest census date (30 September 2021), which were 7.6% and 7.0% respectively^[[NES, 2021](https://turasdata.nes.nhs.scot/data-and-reports/official-workforce-statistics/all-official-statistics-publications/01-march-2022-workforce/dashboards/nhsscotland-workforce/?pageid=6429)]. Although clinical positions had higher vacancy rates on average than non-clinical positions, 13 of the 29 individual roles queried in the survey (44.8%) reported vacancy rates exceeding 10%. These results suggest that vacancies are problematic across a wide variety of role and organisation types. 

The reasons for these recruitment issues are several. First, there does not exist any formalised pathways into careers in drug and alcohol services. A comprehensive evaluation of delivery in health and social care subjects in college settings suggests that many hundreds of people are completing courses every academic year which might prepare them for careers in the drug and alcohol workforce. These range from nursing, psychology and pharmacology, to courses relevant to non-clinical yet still essential roles in advocacy work, social work, counselling and health/social care management. However, partner agencies have confirmed that there is not a single college course currently offered which includes ‘drugs’, ‘alcohol’, ‘addiction’ or ‘substance’ in the course title. Furthermore there is not any public database of course content (such as module names or syllabi) which might offer insights into the relevance of degree programmes to work in drugs and alcohol services.

By contrast, there are dedicated consortia at universities actively progressing empirical research in this space. Moreover several higher education institutions offer interdisciplinary, postgraduate-level degree programmes specifically oriented around substance use. However, the focus of these courses is on training people for roles in academia and/or research rather than frontline services. It is therefore difficult to draw conclusions about trends in the ‘pipeline’ of the potential drug and alcohol workforce.

In addition, a major theme emerging from the qualitative survey responses concerned the general precariousness of secure funding for drug and alcohol posts as well as programmes. This is consistent with the shortfalls facing the wider health and social care sector. Many programmes are funded on a short-term basis, which not only complicates strategic planning efforts, but also often results in low salaries and/or fixed term roles. These factors were reported as a serious deterrent to potential applicants, and services of all types – NHS, health and social care partnerships, third sector services and others – flagged this as an issue.

There was also substantial evidence from the literature review and survey suggesting that negative perceptions of employment in drug and alcohol services are proving detrimental to recruitment efforts for this workforce. As one survey respondent put it, "*...Drug and alcohol services are marginalised and often stigmatised as our service users are, the work that we do, the care and support that we provide is not always understood or appreciated by wider health and social care*". Respondents specifically highlighted how challenging an environment frontline drug and alcohol services can be to work in, and how burnout was affecting word-of-mouth promotion of this sector to prospective applicants. 

The lack of value was also reflected in the types of qualifications available; many people in non-clinical roles undertake qualifications which are not valued by other professions in the same way as a traditional degree, for example Scottish Vocational Qualifications^[[Scottish Qualification Authority, 2022](https://www.sqa.org.uk/sqa/79494.html)]. Greater appreciation for the specialist professional skills required to work in this sector, for example through the recognition of qualifications and training, would confer a level of value that would, in turn, empower drug and alcohol workers to provide treatment and services more effectively.

Finally, survey respondents highlighted that in some cases, the unique perspectives of people with lived experience are not being fully taken advantage of. The skills and experience these people have is often not reflected in the types of roles available to them because they usually lack the necessary qualifications and/or formal work experience. While there are some excellent examples of programmes supporting people with lived experience to enter gainful employment (such as Scottish Drugs Forum's Addiction Worker Training Project^[[SDF, 2022](https://www.sdf.org.uk/what-we-do/addiction-worker-training-project/)]), these remain few in number. People with lived experience have the potential to bring an additional quality and skillset to this challenging work, so developing more opportunities for them to gain the requisite certifications and move into paid roles has the potential to enhance this sector.

Further, those with lived experience were roundly considered a valuable part of the drug and alcohol workforce. An example offered by a survey respondent from the NHS is demonstrative: 

> "*Prior to the COVID 19 we had a volunteer with lived experience who worked alongside the ALN's via third sector organisation Alcohol and Drugs Action. This involved meeting patients in hospital prior to discharge with the aim of engaging them in the recovery community and ADA gorups[sic] and activities. Unfortunately this work was suspended due to hospital visiting restrictions during the pandemic. We are looking at starting this work again as it was successful.*" 

The important role that people with lived experience play in delivering services – along with other frontline staff – ought to be recognised and developed further.


## 3. Retention

In addition to the recruitment matters outlined above, the data emerging from this study also identified serious issues with retaining staff already in the workforce. 

An important element of this is the lack of upward mobility for staff working in drug and alcohol services to progress their career. For example, the survey data showed that the third lowest vacancy rates in the sector were for service managers at 2.1%. The qualitative data also confirmed that progression to higher levels typically happened only when someone left their post. This suggests there is a pinch point for those seeking to proceed to senior roles with a managerial/leadership component, which can lead to operational staff considering employment out with drug and alcohol services.

Closely related to progression is development. Put simply, there are few opportunities for drug and alcohol workers to undertake CPD and upskilling. Granted, the data suggested that enrollments on CPD courses actually increased across a variety of substance abuse topic areas^[These included motivational interviewing, cognitive behavioural therapy, stigma training, trauma-informed practice, and others.] over the last few years, and this was the case for multiple training providers. Some of this may be due to the wholesale migration of training to online mediums. Nevertheless, there is a dearth of formal qualification routes and specialised courses for non-clinical staff (as highlighted above). 

The lack of funding to enroll on courses is also problematic, especially for a sector that is already cash-strapped. One survey respondent called for "*...Increase[d] bursaries to allow more staff to do post graduate study to retain them not just SVQ health and Social care but specialist courses... around addiction and harm reduction different models*". Funding shortfalls also have implications for staff already in the workforce. For example, a reference group participant noted that upskilling band 3 and 4 nursing staff via a higher national certificate scheme was "*...unattractive at present*" because it requires nurses to take a pay cut while they retrain. Finally, respondents highlighted how geography can be a preventative factor for those interested in professional development. However the implications of online delivery for CPD uptake remains to be seen.

Another major issue with retention concerns increasing workloads impacting staff wellbeing. Services are dealing with an ageing population of drug users whose needs are increasingly complex^[see [National Records of Scotland, 2021](https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/deaths/drug-related-deaths-in-scotland/2020)], and consequently caseloads are not only growing larger but more challenging.

Respondents in the survey highlighted how increasing workloads were leading to mental and physical health issues, attrition and burnout amongst frontline workers. Over 10% of respondents discussed how these factors impacted staff sickness, which itself has a cascading effect: overworked employees become progressively more stressed, which leads to them taking more sick days, which then leads to other staff having to manage their caseloads, which leads to stress amongst *those* employees, which results in more sick days, and so on. As one respondent noted:

> "*At one point in summer 2021 the team only had one registered nurse and team lead covering caseload for a team of 8. Team lead is not supposed yo[sic] have a casleoas[sic]. Most of their work was not completed ans[sic] they are also supposed to support another team tooo.. This then lead to burnout of one team members[sic]. The team has always been sitting with vacancies or long term sick since may[sic] 2019 with between 2-6 staff short. Team are also having to heavilly[sic] support 2 band 5 staff as they are both newly qualified... It is very stressful at times*"

Over the six month period from 1 May to 1 November 2021, survey respondents reported a median total of 34 sick days taken per service^[Given the methodological constraints of the survey, it is not possible to compare this figure to wider NHS Scotland sick absences]. Multiple linear regression of this data showed a significant association between caseloads and sick days. When controlling for certain independent variables, a one unit increase in the average caseload per WTE employee increased the number of sick days taken by 5%. This finding suggests that the employee-service user interface is a crucial indicator of staff wellbeing, and has important implications for staff given how volatile caseloads and referrals to drug and alcohol services can be.

## 4. Service design

The drug and alcohol sector also faces challenges in the way services are designed. Drug and alcohol services are commissioned at a local level by the ADPs across Scotland. While this enables services to respond to local needs, it also means that there is wide geographical variation in the way services are designed and what services are available. Certain services are not always available locally, for example residential rehabilitation, or are not available on a full-time basis. In some areas there are not always appropriate services available to which service users can be moved, to the detriment of their wellbeing and recovery. It also has negative consequences on the capacity of services themselves. Services are unable to flow people through systems while they continue receiving new referrals – an issue that is particularly acute in rural areas – and so become increasingly stretched. 
 
Some services operating with higher caseloads reported that this results in systematic challenges as staff have less time to undertake tasks such as recording case notes (a legal requirement for many roles and services). In these circumstances staff may focus on documenting information about issues posing the highest risk, such as child protection, at the expense of factors such as housing or employment. This means staff do not develop a holistic perspective of people’s lives, and opportunities for meaningful intervention are lost. 

There have also been reductions to funding for the sector since 2016. While funding levels have increased in recent years, particularly with the introduction of the National Mission in 2021^[[Scottish Government, 2021b](https://www.gov.scot/policies/alcohol-and-drugs/national-mission/)], this legacy led to some services closing or reducing their capacity. The constrained spending environment in which services are operating emerged from the both the literature and the survey responses as a clear challenge. As noted above this also has negative consequences for recruitment and retention in the sector. 

Issues with staff availability, training and skills hamper the ability to design services, with respect to both capacity and the roles of the different professions. One important factor arising from the literature is the evolution of the role pharmacists play with people who use drugs. Pharmacists have reported engaging in additional training in substance misuse which has built the skills base of this profession. This has not only increased their roles and responsibilities over the past 20 years, but also their confidence in working with this service user group. There is potential for this to be replicated across other professional groups who are not necessarily employed in specialist drug and alcohol services, but who do work with people who use drugs. If more of these professionals – for example GPs, social workers, housing officers, etc – became more skilled and confident in working with people who use drugs, this could potentially alleviate some of the pressure on specialist services.

Finally, the impact of COVID has led to a greater degree of partnership working in some areas, and to services being more flexible in the ways they operate. For example, some organisations reported introducing telephone or online services to help users. These changes could inform service design and delivery going forward.


## 5. Considerations for future work

The drugs and alcohol workforce plays a crucial role delivering specialist health and social care services in Scotland. However, the data collated during this project shows that staff in frontline roles – in addition to already working in challenging environments – regularly face operational and strategic obstacles that impact their ability to deliver for their service users. Moreover, workforce planning efforts must align with the National Workforce Strategy for Health and Social care in Scotland^[[Scottish Government, 2022a](https://www.gov.scot/publications/national-workforce-strategy-health-social-care/)]. Based on this evidence, we therefore propose the following points for consideration.

First, there needs to be improved data capture for this population. NES maintains workforce statistics for every professional category in the NHS, however there is not a tick-box to specifically indicate drug and alcohol workers. This would be a valuable means of collecting trend information on the scope and scale of this workforce, and therefore how and where to start tailoring interventions.

A more long-term aspiration would be to leverage the skills pipeline more effectively. Although the data suggested that there has been increasing CPD uptake in the last few years, survey respondents highlighted the need for more specialised training options. In addition, course offerings in further and higher education settings – especially in colleges – need to be further developed to meet workforce needs. On the university side, the Higher Education Statistics Authority introduced a bespoke ‘drugs and alcohol studies’ category from academic year 2019-20. This will provide insights important on students completing relevant qualifications at the university level. 

Further, consideration must be given to the challenges of excess workloads and burnout within the sector. This arose time and again across different areas of this research project, and coheres with issues facing wider health and social care. However, the capacity problems highlighted here must  be considered alongside the job insecurity that some elements of the workforce consistently face.

There is also a pervasive stigma associated with working in this sector. This is partly the result of a general lack of understanding around what working in frontline services entails. Perhaps more importantly is the perception that working in drug and alcohol settings is somehow less valuable or less important compared to other roles in health and social care. Improved public awareness would not only empower those already working in services, but might also raise the profile of this workforce and thus generate more interest from prospective applicants.

In addition, the value of people with lived experience in delivering frontline services cannot be overstated. Survey respondents and reference group participants alike highlighted that this cohort is a vital part of the workforce, but can sometimes be overlooked or under-appreciated. Going forward, more needs to be done to ensure that people with lived experience have access to appropriate training, and play a more active role in designing services.

Finally the myriad challenges associated with the COVID-19 pandemic and lockdowns have had a lasting impact on drug and alcohol services, in terms of both recruitment and service delivery. However, it also presented opportunities. For example, several services outlined how the pandemic prompted increased inter-organisational collaboration and communication. Because COVID was not the focus of this project, more research is required to collect data on how the pandemic has affected the ability of frontline staff to deliver drug and alcohol services. This could involve examining fluctuations in employment and caseloads, the implications of moving to increased tele-health delivery or changes in prescribing practices.

In any case, the demands on services will no doubt increase as restrictions continue to be relaxed, and services continue working towards embedding Medically Assisted Treatment (MAT) Standards^[[Scottish Government, 2021c](https://www.gov.scot/publications/medication-assisted-treatment-mat-standards-scotland-access-choice-support/)]. Scottish Government has also announced an Opioid Substitution Therapy treatment target, which will be expanded to cover all drugs and alcohol for April 2024^[[Scottish Government, 2022b](https://www.gov.scot/news/treatment-target-for-people-with-problematic-drug-use/)]. The data and evidence outlined here could play a crucial role in informing governance arrangements and strategic thinking around current approaches to recruitment, retention and service design for the drugs and alcohol sector, which will help empower the workforce to more effectively deliver on behalf of service users.
