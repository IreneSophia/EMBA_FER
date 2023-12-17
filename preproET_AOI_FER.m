% This script takes 3D matrices containing areas of interest (eye, 
% forehead, mouth and nose region) and determines how often and long each 
% subject fixates in the respective regions.
%
% (c) I.S. Plank, 10plankp@gmail.com

% clear everything
clearvars;

% specify directories with filesep at the end
dir_in  = [pwd filesep];
dir_vid = 'C:\Users\Oswin\Documents\EMBA_covid\VidStimProcessed\';

% get a list of the files
ls_files = dir(dir_in + "FER-ET*_fixations.csv");


% create empty progress thing
str_progress = sprintf('%s - %2d: |------------------------------------------------------------|',datestr(now,'HH:MM:SS'),length(ls_files));
count        = 1;

%% loop through the subjects
for j = 1:length(ls_files)

    % get the subject ID
    subID = extractBetween(ls_files(j).name, 'FER-ET-', '_fixations.csv');
    subID = subID{1};

    % add to progress bar
    count = count + 1;
    str_progress(count,:)   = repmat(' ', 1, length(str_progress(1,:)));
    str_progress(count,1:14) = sprintf('%s - %2d:',datestr(now,'HH:MM:SS'),j);
    str_progress(count,[16, end])   = '|';

    % load in behavioural data > needed for exact picture shown
    fl_beh = dir(dir_in + "FER-BV-" + subID + "*.csv");
    if ~exist([fl_beh.folder filesep fl_beh.name], 'file')
        continue
    end
    tbl_beh = readtable([fl_beh.folder filesep fl_beh.name]);

    %% fixations

    % check if the AOI file already exists
    fl_fixAOI = [ls_files(j).folder filesep ls_files(j).name(1:end-4) '_AOI.csv'];

    if ~exist(fl_fixAOI, "file")

        % if it does not exist load in fixations and only keep those that 
        % started during a trial
        tbl_fix = readtable([ls_files(j).folder filesep ls_files(j).name]);
        tbl_fix = tbl_fix(~isnan(tbl_fix.on_trialNo),:);
        tbl_fix.AOI = strings(height(tbl_fix),1);

        % since the AOI matrices are too large to have all in memory, we loop
        % through the fixations and load only the one that we need
        for i = 1:height(tbl_fix)
    
            % adjust progress bar
            x = floor(30*i/height(tbl_fix));
            str_progress(count,17:(16+x)) = '-';
            clc;
            disp(str_progress)
    
            % check if it is still the same trial
            if i > 1
                if tbl_fix.on_trialNo(i) ~= tbl_fix.on_trialNo(i-1)
                    vid = tbl_beh{tbl_beh.trl == tbl_fix.on_trialNo(i), "video"}{1};
                    load([dir_vid vid '.mat'])
                end
            % if it is the first fixation, always load the info
            else
                vid = tbl_beh{tbl_beh.trl == tbl_fix.on_trialNo(i), "video"}{1};
                load([dir_vid vid '.mat'])
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
    
        writetable(tbl_fix, fl_fixAOI)

    end

    %% saccades

    % check if saccade file exists
    fl_sac = dir(dir_in + "FER-ET-" + subID + "*_saccades.csv");
    if isempty(fl_sac)
        continue
    end

    % check if the AOI file already exists
    fl_fixAOI = [fl_sac.folder filesep fl_sac.name(1:end-4) '_AOI.csv'];
    if exist(fl_fixAOI, "file")
        continue
    end

    % if it does not exist, start by loading the data and getting rid of
    % saccades outside of a stimulus presentation
    tbl_sac = readtable([fl_sac.folder filesep fl_sac.name]);
    tbl_sac = tbl_sac(~isnan(tbl_sac.on_trialNo),:);
    tbl_sac.on_AOI  = strings(height(tbl_sac),1);
    tbl_sac.off_AOI = strings(height(tbl_sac),1);

    for i = 1:height(tbl_sac)

        % adjust progress bar
        x = floor(30*i/height(tbl_sac));
        str_progress(count,47:(46+x)) = '-';
        clc;
        disp(str_progress)

        % check if it is still the same trial
        if i > 1
            % if not, then load the corresponding AOI matrix
            if tbl_sac.on_trialNo(i) ~= tbl_sac.on_trialNo(i-1)
                vid = tbl_beh{tbl_beh.trl == tbl_sac.on_trialNo(i), "video"}{1};
                load([dir_vid vid '.mat'])
            end
        % if it is the first saccades, always load the info
        else
            vid = tbl_beh{tbl_beh.trl == tbl_sac.on_trialNo(i), "video"}{1};
            load([dir_vid vid '.mat'])
        end
        % get the frame number
        fno = str2double(tbl_sac.on_trialStm{i}(5:end));
        % get the correct indices: since the pictures are presented in the
        % middle of the screen twice their side, we need to account for
        % that
        x   = round((tbl_sac.on_xPixel(i) - 718)/2);
        y   = round((tbl_sac.on_yPixel(i) - 38)/2);
        % only continue if coordinates within AOI matrix
        if x >= 1 && x <= size(mtx_aois,2) && y >= 1 && y <= size(mtx_aois,1)
            if mtx_aois(y, x, fno) == 0.81
                tbl_sac.on_AOI(i) = "eyes";
            elseif mtx_aois(y, x, fno) == 0.82
                tbl_sac.on_AOI(i) = "mouth";
            elseif mtx_aois(y, x, fno) == 0.84
                tbl_sac.on_AOI(i) = "nose";
            elseif mtx_aois(y, x, fno) == 0.86
                tbl_sac.on_AOI(i) = "fore";
            end
        end
        % get the new frame number, if still within a trial
        if ~isempty(tbl_sac.off_trialStm{i})
            fno = str2double(tbl_sac.off_trialStm{i}(5:end));
        end
        % get the correct indices: since the pictures are presented in the
        % middle of the screen twice their side, we need to account for
        % that
        x   = round((tbl_sac.off_xPixel(i) - 718)/2);
        y   = round((tbl_sac.off_yPixel(i) - 38)/2);
        % only continue if coordinates within AOI matrix
        if x >= 1 && x <= size(mtx_aois,2) && y >= 1 && y <= size(mtx_aois,1)
            if mtx_aois(y, x, fno) == 0.81
                tbl_sac.off_AOI(i) = "eyes";
            elseif mtx_aois(y, x, fno) == 0.82
                tbl_sac.off_AOI(i) = "mouth";
            elseif mtx_aois(y, x, fno) == 0.84
                tbl_sac.off_AOI(i) = "nose";
            elseif mtx_aois(y, x, fno) == 0.86
                tbl_sac.off_AOI(i) = "fore";
            end
        end
    end

    writetable(tbl_sac, fl_fixAOI)


end