# Math-Support-Centre-

This repository contains code to analysis Maths Support Centre data and produce an RShiny forecasting app for waiting times.
A brief overview was presented at CETL-MSOR 2019 in DCU.

Stage 1: From the "Original.csv" file, use the files "Reformatting_unedited_MSC_file.R" and "Reformatting_term_week_and_number_of_tutors.R" to create a usable MSC file for data analytics ("MSC.csv").

Stage 2: Using the "MSC.csv" file, run the "Diagrams_for_waiting_times.R" code. This will produce waiting times diagrams. 

Stage 3: The "Prediction_code.R" can be used to to idetify the best prediction algorithm for waiting times of six algorithms and using the random forest prediction method in conjunction with the "example tutor timetable.csv" file, predict the waiting time for every time in Semester 1 of 2019.

Stage 4: The "ui.R", "server.R", and "RforAp.RData" files make up an RShiny waiting time prediction ap which students in UCD were able to access in 2018.
