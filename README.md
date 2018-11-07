# Project: Can you unscramble a blurry image? 
![image](figs/example.png)

### [Full Project Description](doc/project3_desc.md)

Term: Fall 2018

+ Team # 3
+ Team members
	+ Yunfan Li
	+ Mingyu Yang
	+ Peiqi Jin
	+ Shichao Jia
	+ Yanchen Chen

+ Project summary: Our goal for this project is to create a classification engine in order to enhance the resolution of images. We use 1500 pairs of low and high resolution image to train our models. PSNR is used to evaluate the performance of each model. We randomly sample 1000 points and extracte their eight surrounding pixels as features and use the corresponding pixels in high resolution image as response. we use GBM to train our baseline model and parameters are tuned to select the best performing one. To achieve better prediction, we use also use xgboost to retrain a more advanced model. Running time and model performance both improved with our advance model. 
	
**Contribution statement**: ([default](doc/a_note_on_contributions.md)) All team members approve our work presented in this GitHub repository including this contributions statement. Each member complete the following task: 

Baseline Model: Yunfan Li, Mingyu Yang  
Advanced Model: Peiqi Jin  
PPT and implementation: Shichao Jia  
Parrallelization of functions: Yanchen Chen  

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
