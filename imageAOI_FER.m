% clear everything
clearvars;

% get a list of the files and filter out the animal files
ls_files = dir("VidStimProcessed" + filesep + "*.mat");

% create empty progress thing
str_progress = '        |------------------------------------------------------------|';
count        = 1;

% loop through the list
for j = 1:length(ls_files)

    % get video name
    vid = ls_files(j).name(1:end-4);

    % check if the folder already exists
    dir_vid = ['VidStimProcessed' filesep vid];
    if exist(dir_vid, 'dir') > 0
        continue
    end

    % add to progress bar
    count = count + 1;
    str_progress(count,:)   = repmat(' ', 1, length(str_progress(1,:)));
    str_progress(count,1:6) = vid;
    str_progress(count,7)   = ':';
    str_progress(count,[9, end])   = '|';

    % load the data
    load(['VidStimProcessed' filesep ls_files(j).name]);

    % create the directory
    mkdir(dir_vid);

    % loop through the frames
    for i = 1:size(mtx_aois,3)

        % adjust progress bar
        x = floor(60*i/300);
        str_progress(count,10:(9+x)) = '-';
        clc;
        disp(str_progress)

        % read in the picture ADJUST AFTER GETTING PICTURES!
        img = imread(['VidStimProcessed' filesep 'AF01AF_000.jpg']);

        % set all ROI values to 1
        mtx = mtx_aois(:,:,i);
        mtx(mtx > 0) = 1;

        % convert to 3 colours
        mtx = repmat(im2uint8(mtx),[1 1 3]);

        % perform the blending
        blended = mtx*0.5 + img;

        % save it as a jpg
        imwrite(blended, sprintf('%s%s%s_%03d.png',...
            dir_vid, filesep, vid, i));

    end

end