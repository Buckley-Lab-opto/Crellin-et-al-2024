%% PIV analysis 
% 1. save images as image sequence using batch-save-as-img-sequence.ijm
% macro
% 2. perform weiner2 filter and 3x moving average on raw GFP images
% 2. run PIV analysis in PIVlab
% 3. run plot PIV
% Note: Run by section (not Run to End)

% Bugs: Error message - error using OpenGL for printing due to -nodisplay  
% caused by parfor in plot_piv_results (parallel processing done without 
% display of figures incompatible with printing figures - possibly resolve
% with .fig creation then conversion outside parfor

%% Run this section prior to any sections below 

addpath(genpath('/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/')); % path where the matlab functions are
currentDate = datestr(now, 'yyyy_mmm_dd');

%% Iterates within the folder to save weiner filtered and moving averages for all image sequences
% Use batch-save-as-img-sequence.ijm macro first to have files saved in
% correct folder structure (e.g. .../embryo_id/image-sequence/img_XXXX.tif)

path = '/Volumes/rfs-ceb85-buckley-lab-xzl8UdsANeo/Helena/to be organised/2024-10-04/data'; % put path to image sequences here

folders = dir(fullfile(path));

for i = 1:numel(folders)
    
    folder = folders(i).name;

    if ~matches(folder,[".","..",".DS_Store"]) % add any other folders here that you dont want to process (check folders variable for hidden folders)

    folder_path = fullfile(path, folder);
    image_folder = fullfile(folder_path, 'image-sequence');
    out_folder = fullfile(folder_path, 'moving_average');
    disp(image_folder);
    disp(out_folder);

     if not(isfolder(out_folder)) %~exist(out_folder, 'dir')
         mkdir(out_folder);
     end

    eR = expReader(image_folder);
    
    for t_id = eR.timePoints()
        I = eR.readSpecificImage(t_id);
        J = zeros(size(I));
        for c = -1:1
            time = min(max(t_id+c,1),eR.timePoints(end));
            I = double(eR.readSpecificImage(time));
            [I, ~] = wiener2(I, [5 5]);
            J = J + I;
        end
        J = round(J ./ 3);
        J = uint8(J);
        
        imwrite(J, fullfile(out_folder, ['img_' num2str(t_id, '%04d') '.tif']));
    end
    
    disp(['Moving average finished for :', folder]);
    end
end

disp('Done!');
beep;

%% Next perform PIV in PIVlab GUI (in Apps)
% Draw mask over neural tissue, apply to all and run with default PIV settings
% Save files with default names in separate folders for each embryo

%% Initialise parameters before PIV data processing and plotting in MATLAB
% Do not change (except workers)

vel_mean = zeros(20, 1000, 5);
vel_std = zeros(20, 1000, 5);
vang_mean = zeros(20, 1000, 5);
vang_std = zeros(20, 1000, 5);
iso_mean = zeros(20, 1000, 5);
iso_std = zeros(20, 1000, 5);
iso_x_mean = zeros(20, 1000, 5);
iso_x_std = zeros(20, 1000, 5);
iso_y_mean = zeros(20, 1000, 5);
iso_y_std = zeros(20, 1000, 5);
ani_mean = zeros(20, 1000, 5);
ani_std = zeros(20, 1000, 5);
aniang_mean = zeros(20, 1000, 5);
aniang_std = zeros(20, 1000, 5);
str_mean = zeros(20, 1000, 5);
str_std = zeros(20, 1000, 5);
apstr_mean = zeros(20, 1000, 5);
apstr_std = zeros(20, 1000, 5);
mlstr_mean = zeros(20, 1000, 5);
mlstr_std = zeros(20, 1000, 5);

idx = 0;

% parpool(workers)
% workers = 8; % set this to number of cores available for parallel processing 

%% Data processing and plotting
% Repeat this section for each timelapse
% Input parameters below for the timelapse

param = struct();
param.pixel_s = 0.275; % pixel size [µm/pix]
param.time_step = 5; % [seconds (/timepoint)]
param.spatial_filt_size = 5; % spatial averaging [no of vectors]
param.time_filt_size = 1; % temporal averaging [no of timepoints]
param.on_time_point = 1; % activation start 
param.off_time_point = 361; % activation end
param.xroi = 0;
param.yroi = 0;
param.initial_time = 1; % first timepoint for cumulative strain rate visualisation
param.final_time = 360; % last timepoint for cumulative strain rate visualisation
param.max_value = 0.025; % controls color scale (max str value)
param.min_value = 0; % controls color scale (min str value)


experiment_folder = '/Volumes/rfs-ceb85-buckley-lab-xzl8UdsANeo-1/Helena/to be organised/2024-10-04/2024-10-04_LARG-Krox20-19ss_E8';
image_path = '/Volumes/rfs-ceb85-buckley-lab-xzl8UdsANeo-1/Helena/to be organised/2024-10-04/cropped/2024-10-04_LARG-Krox20-19ss_E8.tif';
mask_file = [experiment_folder filesep 'PIVlab_mask.mat'];
piv_path = [experiment_folder filesep 'PIVlab.mat'];
path2roi = [experiment_folder filesep 'region.bmp'];
out_folder = [experiment_folder filesep currentDate '_results_sp_' num2str(param.spatial_filt_size) '_tf_' num2str(param.time_filt_size) ];

data_folder = [experiment_folder filesep 'xlsx_data']; %
 if not(isfolder(data_folder)) %~exist(data_folder, 'dir')
     mkdir(data_folder); 
 end
expname = strsplit(experiment_folder, '/');
expdate = expname{end-1};
expname = strsplit(expname{end}, '.tif');
expname = expname{1};
xlsx_folder = [experiment_folder filesep 'xlsx_data' filesep currentDate]; 
if not(isfolder(xlsx_folder)) %~exist(xlsx_folder, 'dir')
     mkdir(xlsx_folder); 
end 

plot_piv_results(image_path, mask_file, piv_path, path2roi, out_folder, param, experiment_folder);

path2datagrid =[out_folder filesep 'grid_data.mat']; %

idx = idx+1;
 [vel_mean, vel_std, vang_mean, vang_std, iso_mean, iso_std, iso_x_mean, iso_x_std, iso_y_mean, iso_y_std, ani_mean, ani_std, aniang_mean, aniang_std, str_mean, str_std, apstr_mean, apstr_std, mlstr_mean, mlstr_std, T] = grabNSWECdata(image_path, piv_path, path2roi, path2datagrid, param, idx, vel_mean, vel_std, vang_mean, vang_std, iso_mean, iso_std, iso_x_mean, iso_x_std, iso_y_mean, iso_y_std, ani_mean, ani_std, aniang_mean, aniang_std, str_mean, str_std, apstr_mean, apstr_std, mlstr_mean, mlstr_std);
writetable(T, [xlsx_folder filesep expdate '_' expname '_CNSEW.xlsx']);

plot_cumulative_strain_rate(image_path, mask_file, piv_path, path2roi, out_folder, param);

disp("finished-0");
beep;



%% Data processing and plotting
% Repeat this section for each timelapse
% Input parameters below for the timelapse

param = struct();
param.pixel_s=0.275; % [um (/pix)]
param.time_step=5; %[s (/timePoint)]
param.spatial_filt_size = 5;
param.time_filt_size = 1;
param.on_time_point = 26;
param.off_time_point = 51;
param.xroi = 0;
param.yroi = 0;
param.initial_time = 26; % first time point for cumulative strain rate
param.final_time = 37; % last time point for cumulative strain rate
param.max_value = 0.025; % control color scale (max str value)
param.min_value = 0; % control color scale (min str value)

experiment_folder = '/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/data/2022-10-18/E1_new_15ss_470nm_TL_region1.tif';
image_path = ['/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/data/2022-10-18/E1_new_15ss_470nm_TL_region1.tif/image sequence/E1_new_15ss_470nm_TL_region1_-5.7rot.tif_GFP.tif'];
mask_file = [experiment_folder filesep 'PIVlab_mask.mat'];
piv_path = [experiment_folder filesep 'PIVlab.mat'];
path2roi = [experiment_folder filesep 'roi-02_-5.7rot.bmp'];
out_folder = [experiment_folder filesep currentDate '_results_sp_' num2str(param.spatial_filt_size) '_tf_' num2str(param.time_filt_size) ];

data_folder = [experiment_folder filesep 'xlsx_data']; %
 if not(isfolder(data_folder)) %~exist(data_folder, 'dir')
     mkdir(data_folder); 
 end
expname = strsplit(experiment_folder, '/');
expdate = expname{end-1};
expname = strsplit(expname{end}, '.tif');
expname = expname{1};
xlsx_folder = [experiment_folder filesep 'xlsx_data' filesep currentDate]; 
if not(isfolder(xlsx_folder)) %~exist(xlsx_folder, 'dir')
     mkdir(xlsx_folder); 
end 

plot_piv_results(image_path, mask_file, piv_path, path2roi, out_folder, param, experiment_folder);

path2datagrid =[out_folder filesep 'grid_data.mat']; %

idx = idx+1;
 [vel_mean, vel_std, vang_mean, vang_std, iso_mean, iso_std, iso_x_mean, iso_x_std, iso_y_mean, iso_y_std, ani_mean, ani_std, aniang_mean, aniang_std, str_mean, str_std, apstr_mean, apstr_std, mlstr_mean, mlstr_std, T] = grabNSWECdata(image_path, piv_path, path2roi, path2datagrid, param, idx, vel_mean, vel_std, vang_mean, vang_std, iso_mean, iso_std, iso_x_mean, iso_x_std, iso_y_mean, iso_y_std, ani_mean, ani_std, aniang_mean, aniang_std, str_mean, str_std, apstr_mean, apstr_std, mlstr_mean, mlstr_std);
writetable(T, [xlsx_folder filesep expdate '_' expname '_NSWEC.xlsx']);

plot_cumulative_strain_rate(image_path, mask_file, piv_path, path2roi, out_folder, param);

disp("finished-1");
beep;


