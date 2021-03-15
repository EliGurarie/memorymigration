Modeling tactical and memory-driven migration in dynamic environments
========================================================
author: Sriya Potluri and Elie Gurarie
date: March 2021
autosize: true
transition: none
width: 1600
height: 900


Premise
========================================================

To optimally forage in a dynamic environment, you need both **tactical** responses (adapting to local changes in resource gradients), and **strategic** responses (using memory to guide long-term decisions). 

***





Model
========================================================

$${\frac{\partial u(x,t)}{\partial t}} = -\varepsilon {\frac{\partial^2 u(x,t)}{\partial x^2}} + \alpha \frac{\partial h(x,t)}{\partial x} + \sum_{i = 0}^n \beta_i \frac{\partial u(x, t - i\tau)}{\partial x}$$

- $\epsilon$: diffusion, exploration / search
- $\alpha$: taxis, resource followign
- $\beta_0$: cohesion, tendency to stick to group
- $\beta_1$: memory - tendency to do what was done previous year

Seasonal resource
========================================================

$$R(x,t, \theta) = \chi B(x/\chi, a(t, \theta), b(t, \theta))$$

<!-- make short--> 

1. The total amount constant.
2. At the beginning, middle, and end of the year the resource is uniformly distributed. 
3. 
4. 


![](images/exampleResourcePlots-1.png)


Running the model 
==========================

- R package `deSolve` and `ReacTran` - fluid transport pde solvers.
- Functions all in `memorymigration` GitHub project. 
- SESYNC cluster 
- server-based Rstudio



Metrics
======================


1. Social cohesiveness
2. Foraging Efficiency
3. Migratoriness


Metrics: Cohesiveness (problematic!)
==========================

Definition 

***

Image


Metrics: Foraging Efficiency 
==========================



Metrics: Migratoriness
========================



Questions that this model can answer: 
======================


Q1. What's the "best" parameter combination to maximize FE - how does that affect Migratoriness and Cohesiveness - under various resource scenarios.

Q2. Can migration emerge from a non-migratory initial condition? 

Q3. As conditions CHANGE, how resilient is the population at various Diffusion, Advection, Cohesion, Memory values?  
- spatial drift
- temporal drift
- increased variability

***

![](images/brainstorm.png)


Preliminary results: 
================================


