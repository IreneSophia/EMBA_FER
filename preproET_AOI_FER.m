% This script takes 3D matrices containing areas of interest (eye, 
% forehead, mouth and nose region) and determines how often and long each 
% subject fixates in the respective regions.
% takes about 140s per subject
% (c) I.S. Plank, 10plankp@gmail.com

% clear everything
clearvars;

% get a list of the files
ls_files = dir("FER-ET*_fixations.csv");


% create empty progress thing
str_progress = sprintf('%s - %2d: |------------------------------------------------------------|',datestr(now,'HH:MM:SS'),length(ls_files));
count        = 1;

for j = 1%:length(ls_files)

    % get the subject ID
    subID = extractBetween(ls_files(j).name, 'FER-ET-', '_fixations.csv');
    subID = subID{1};

    % check if the AOI file already exists
    fl_AOI = [ls_files(j).folder filesep ls_files(j).name(1:end-4) '_AOI.csv'];
    if exist(fl_AOI, "file")
        continue
    end

    % add to progress bar
    count = count + 1;
    str_progress(count,:)   = repmat(' ', 1, length(str_progress(1,:)));
    str_progress(count,1:14) = sprintf('%s - %2d:',datestr(now,'HH:MM:SS'),j);
    str_progress(count,[16, end])   = '|';

    % load in fixations and only keep those that started during a trial
    tbl_fix = readtable([ls_files(j).folder filesep ls_files(j).name]);
    tbl_fix = tbl_fix(~isnan(tbl_fix.on_trialNo),:);
    tbl_fix.AOI = strings(height(tbl_fix),1);

    % load in behavioural data > needed for exact picture shown MISSING
    

    % since the AOI matrices are too large to have all in memory, we loop
    % through the fixations and load only the one that we need
    for i = 1:height(tbl_fix)

        % adjust progress bar
        x = floor(60*i/height(tbl_fix));
        str_progress(count,10:(9+x)) = '-';
        clc;
        disp(str_progress)

        % check if it is still the same trial
        if i > 1
            if tbl_fix.on_trialNo(i) ~= tbl_fix.on_trialNo(i-1)
                vid = 'BM31AN';
                load(['VidStimProcessed' filesep vid '.mat'])
            end
        % if it is the first fixation, always load the info
        else
            vid = 'BM31AN';
            load(['VidStimProcessed' filesep vid '.mat'])
        end
        % get the frame number
        fno = str2double(tbl_fix.on_trialStm{i}(5:end));
        % get the correct indices: since the pictures are presented in the
        % middle of the screen twice their side, we need to account for
        % that
        x   = round((tbl_fix.meanX_pix(i) - 718)/2);
        y   = round((tbl_fix.meanY_pix(i) - 38)/2);
        if mtx_aois(y, x, fno) == 0.81
            tbl_fix.AOI(i) = "eyes";
        elseif mtx_aois(y, x, fno) == 0.82
            tbl_fix.AOI(i) = "mouth";
        elseif mtx_aois(y, x, fno) == 0.84
            tbl_fix.AOI(i) = "nose";
        elseif mtx_aois(y, x, fno) == 0.86
            tbl_fix.AOI(i) = "fore";
        end
    end

    writetable(tbl_fix, fl_AOI)

end