% This script takes 3D matrices containing areas of interest (eye, 
% forehead, mouth and nose region) as well as the original image and 
% creates an image overlayed with the AOIs to check for fit. 
% (c) I.S. Plank, 10plankp@gmail.com

% clear everything
clearvars;

% get a list of the files and filter out the animal files
ls_files = dir("VidStimProcessed" + filesep + "*.mat");

% create empty progress thing
str_progress = '        |------------------------------------------------------------|';
count        = 1;

% create the colourmap
cm = zeros(256,3);
cm(208,:) = [230/255 159/255 0      ];
cm(215,:) = [86/255  180/255 233/255];
cm(220,:) = [0       158/255 115/255];
cm(225,:) = [204/255 121/255 167/255];

% loop through the list
for j = 1:length(ls_files)

    % get video name
    vid = ls_files(j).name(1:end-4);

    % check if the folder already exists
    dir_vid = ['VidStimProcessed' filesep vid];
    if exist(dir_vid, 'dir') > 0
        if length(dir([dir_vid filesep '*.png'])) == 300
            continue
        end
    else
        % create the directory
        mkdir(dir_vid);
    end

    % add to progress bar
    count = count + 1;
    str_progress(count,:)   = repmat(' ', 1, length(str_progress(1,:)));
    str_progress(count,1:6) = vid;
    str_progress(count,7)   = ':';
    str_progress(count,[9, end])   = '|';

    % load the data
    load(['VidStimProcessed' filesep ls_files(j).name]);

    % loop through the frames
    for i = 1:size(mtx_aois,3)

        % adjust progress bar
        x = floor(60*i/300);
        str_progress(count,10:(9+x)) = '-';
        clc;
        disp(str_progress)

        % read in the picture
        fl  = dir(sprintf('%s\\taskFER\\Stimuli%s%s*%03d.jpg',...
            pwd, filesep, vid, i-1));
        img = imread([fl.folder filesep fl.name]);

        % different colours for the different regions of interest
        mtx = im2uint8(ind2rgb(im2uint8(mtx_aois(:,:,i)),cm));

        % perform the blending
        blended = mtx*0.5 + img;

        % save it as a jpg
        imwrite(blended, sprintf('%s%s%s_%03d.png',...
            dir_vid, filesep, vid, i));

    end

end