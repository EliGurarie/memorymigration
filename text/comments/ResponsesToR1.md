 EVALUATION
#### Please summarize the main findings of the study.

> This paper presents a simulation study of a model of animal migration. The authors formulate a partial differential equation that describes the time evolution of an animal population inhabiting a stylized one-dimensional dynamic environment. The pde model is an advection-diffusion equation that includes a resource tracking term and a social attraction force that pushes individuals toward the population centroid. The migration is introduced as an external forcing that models alternating travel between two separate habitats with both a reference memory and short term memory of the migration route.

> With this model the authors explore the effects of various parameter combinations and possible future scenarios such as shifting habitats due to climate change.


#### Please highlight the limitations and strengths.

> The main limitation of the paper lies with the highly abstract model. This is purely a simulation study in 1-dimension that by necessity makes severe simplifying assumptions when modelling sociality, memory, and resource tracking. Some of the modelling choices seem hard to justify - notably the social interaction range is often very large compared to the migration distance.

Our model is rightly labeled as "abstract" by the reviewer, in the sense that we attempted (really, labored) to formulate virtually the simplest possible model would show the behaviors we were interested in exploring.  We note - here - that we tried about a half dozen other formulations -- some of which were simpler but simply did not function (in a way, this - too - is a result, though a difficult one to formalize).  Regarding the large upper limit of spatial scale of "sociality" - essentially, the largest spatial scale corresponded to a scenario where there is, (nearly) no social cohesion whatsoever, i.e. the movement is mainly governed by resource attraction and the migration memory process.  Very small spatial scales of sociality are incapable of "learning" anything about the resource distribution due to highly localized information gathering, another more or less fundamental result **which we do a better job of highlighting in the revised discussion**. 

> The strengths of the paper are in its clear exposition - it uses the simple model developed to address some important and timely questions, and focuses on what are likely to be the key factors that determine migratory behaviour in real systems.

#### Please comment on the methods, results and data interpretation. If there are any objective errors, or if the conclusions are not supported, you should detail your concerns.

> I believe the conclusions are in large part supported by the results, although I have some concerns about some of the terminology which I detail below. The simulation methods and analysis seem fine to me.

Thank you ... see responses below. 

#### Please provide your detailed review report to the editor and authors (including any comments on the Q4 Check List):

> Beyond the answers above I have the following comments on the paper.

> 1 - the authors appear to be using a very general definition of migration. If I'm understanding their results correctly I think a lot of people would disagree with their characterisation of 'the spontaneous emergence of a migratory behavior'. If individuals are responding to local cues is this really migration? If there is a suppression of response to local cues that would typically be associated with migration can the authors clarify how this emerges?

We modified the rather strong (and easy to misinterpret) term "spontaneous emergence of a migratory behavior" with "the emergence of a migratory behavior from an essentially resident or naive initial condition."  What we are specifically referring to here is the scenario illustrated in figure 5.  This scenario is initialized with a *nearly* migratory initial condition (i.e. the distance between the seasonal ranges was 1). This slight initial condition was necessary because in our model, *some* seasonal shift must be estimated to seed the beginnings of a consistent, long-distance migration.  Otherwise, we are not quite clear on the reviewer's suggestion that response to "local cues" does not constitute migration.  In our model, those pieces of information are the mechanism by which a long-distance migration is - eventually - learned.  The point of the "learning to migrate" exercise is, mainly, to show how difficult (i.e. with what a narrow range of parameter values) the system can find a self-reinforcing way to expand its exploration while still retaining enough social fidelity for the population - at large - to learn to migrate. 

> 2 - the increase in resource tracking ability due to social interactions is a well-studied phenomena, not only in animal movement but also in swarm robotics and heuristic algorithms. There's a large literature here that is almost entirely neglected in the current work. It would be good if the authors could better place their findings relating to social taxis within the context of previous research.

**Thank you for this comment.  We have added references - and summarized some insights - from ABC, DEF, GHI in the revised Discussion.**

> 3 - I was a little confused by the implementation of the reference memory. For me, it would make far more sense to think in terms of a long-term memory e.g. that changes over a decadal timescale, and a short-term memory that recalls the previous year. Instead, here the reference memory is prespecified for year zero and fixed. I also found equation 3 unclear. Is the y-subscript on kappa an exponent? Does this mean that the reference migration route is gradually forgotten? I found it hard to reconcile this equation with the results presented in sec3.5 that consisted of 30 years of simulations with kappa~0.6-0.8.

Kappa is an exponent.  But you're right - it is not to most satisfactory. 

> I noticed a few other typos:
> L48 missing reference

fixed

> L131 I'm not sure why this is a delta t' in the integral rather than dt'

good catch, thank you. 

> L142 bracket misplaced for h(x,t)

fixed

> L142 year sq

fixed

> L155 note/not

fixed

> Figures - it would be helpful to label the figures with a,b,c if using these labels in the caption.

**to do**