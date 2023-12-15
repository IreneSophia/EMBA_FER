% This function preprocesses eye tracking data for the FER task collected
% with a LiveTrack Lightning Eye Tracker. It takes the filename of the csv
% and the path of the file as input. All data is saved in the same folder
% as the input file. 
% 
% Detection of events (blinks, saccades, glissades and fixations) is based
% on NYSTRÖM & HOLMQVIST (2010).
%
% (c) Irene Sophia Plank 10planki@gmail.com

function FER_preproET(filename, dir_path)

%% read in data and calculate values

% get subject ID from the filename
subID = convertStringsToChars(extractBefore(filename,"_"));
fprintf('\nNow processing subject %s.\n', extractAfter(subID,'-'));

% set options for reading in the data
opts = delimitedTextImportOptions("NumVariables", 11);
opts.DataLines = [2, Inf];
opts.Delimiter = ",";
opts.VariableNames = ["timestamp", "trigger", "leftScreenX", "leftScreenY",...
    "rightScreenX", "rightScreenY", "leftPupilMajorAxis", "leftPupilMinorAxis",...
    "rightPupilMajorAxis", "rightPupilMinorAxis", "comment"];
opts.VariableTypes = ["double", "double", "double", "double", "double",...
    "double", "double", "double", "double", "double", "string"];

tbl = readtable([dir_path filesep filename], opts); 
tbl.pupilDiameter = mean([tbl.leftPupilMajorAxis,tbl.leftPupilMinorAxis],2);
tbl.tracked = bitget(tbl.trigger,14-3); 
tbl.pupilDiameterRight = mean([tbl.rightPupilMajorAxis,tbl.rightPupilMinorAxis],2);
tbl.trackedRight = bitget(tbl.trigger,15-3); 

%% add trial information

% total number of trials
not = 64;

% find trial indices
trl = tbl.comment(~ismissing(tbl.comment));
trl = split(trl, "_");
idx = find(extractBefore(tbl.comment,4) == "pic");

if not ~= size(unique(trl(:,2)),1)
    warning("This FER dataset does NOT have the correct amount of 64 trials!")
end

% add another row at the end
if height(tbl)-idx(end,1) < 500
    last = height(tbl)-idx(end,1);
else
    last = 10;
end
idx(end+1,:) = idx(end,1)+last;

% create empty columns to be filled with information
tbl.trialNo   = strings(height(tbl),1);
tbl.trialStm  = strings(height(tbl),1);
tbl.trialEmo  = strings(height(tbl),1);

% loop through the trials and add the information from the comment: 
% "trial", "pic", "emotion"
for i = 1:(size(idx,1)-1)

    % check if this is the last pic of a trial
    if i == (size(idx,1)-1)
        last = idx(i,1) + 10; % if last pic at all, take 20ms
    elseif trl(i,2) ~= trl(i+1,2)
        last = idx(i,1) + 10; % if yes, take 20ms
    else
        last = idx(i+1,1)-1;  % if no, take all until the next one
    end
    % trial number
    tbl.trialNo(idx(i,1):last)   = trl(i,2);
    % trial stimulus (exact pic)
    tbl.trialStm(idx(i,1):last)  = trl(i,1)+"_"+trl(i,3);
    % trial emotion
    tbl.trialEmo(idx(i,1):last)   = trl(i,4);

end

%% classification of events

% generate parameters for NH2010 classification code.
ETparams = defaultParameters;
ETparams.screen.resolution              = [2560 1600];   % screen resolution in pixel
ETparams.screen.size                    = [0.344 0.215]; % screen size in m
ETparams.screen.viewingDist             = 0.57;          % viewing distance in m
ETparams.screen.dataCenter              = [ 0 0];        % center of screen has these coordinates in data
ETparams.screen.subjectStraightAhead    = [ 0 0];        % specify the screen coordinate that is straight ahead of the subject. Just specify the middle of the screen unless its important to you to get this very accurate!

% format gaze directions as screen pixel coordinates for NH2010
tbl.xPixel = tbl.leftScreenX*(ETparams.screen.resolution(1)/(ETparams.screen.size(1)*1000));
tbl.yPixel = tbl.leftScreenY*(ETparams.screen.resolution(2)/(ETparams.screen.size(2)*1000));

% run the NH2010 classifier code on full data set
[classificationData,ETparams]   = runNH2010Classification(...
    tbl.xPixel,tbl.yPixel,tbl.pupilDiameter,ETparams);

% merge glissades with saccades
classificationData = mergeSaccadesAndGlissades(classificationData);
if isfield(classificationData,'glissade')
    classificationData = rmfield(classificationData,'glissade');    
end

%% create output tables

% fixations
fn = fieldnames(classificationData.fixation);
for k = 1:numel(fn)
    if size(classificationData.fixation.(fn{k}),1) < size(classificationData.fixation.(fn{k}),2)
        classificationData.fixation.(fn{k}) = classificationData.fixation.(fn{k}).';
    end
end
tbl_fix = struct2table(classificationData.fixation);

% saccades
fn = fieldnames(classificationData.saccade);
for k = 1:numel(fn)
    if size(classificationData.saccade.(fn{k}),1) < size(classificationData.saccade.(fn{k}),2)
        classificationData.saccade.(fn{k}) = classificationData.saccade.(fn{k}).';
    end
end
% sometimes offsetVelocityThreshold is too long, then remove excess entries
n_sac = size(classificationData.saccade.on);
classificationData.saccade.offsetVelocityThreshold = ...
    classificationData.saccade.offsetVelocityThreshold(1:n_sac);
classificationData.saccade.peakVelocityThreshold = repmat( ...
    classificationData.saccade.peakVelocityThreshold, ...
    size(classificationData.saccade.peakVelocity,1), ...
    size(classificationData.saccade.peakVelocity,2));
classificationData.saccade.onsetVelocityThreshold = repmat( ...
    classificationData.saccade.onsetVelocityThreshold, ...
    size(classificationData.saccade.peakVelocity,1), ...
    size(classificationData.saccade.peakVelocity,2));
tbl_sac = struct2table(classificationData.saccade);

%% add trial information for on and off to the event tables

% add an index row to the data table
tbl.on  = (1:height(tbl)).';
tbl.off = (1:height(tbl)).';

% add event info to fixations
cols    = ["trialNo","trialStm","trialEmo"];
tbl_fix = join(tbl_fix,tbl(:,["on",cols]));
newNames = append("on_",cols);
tbl_fix = renamevars(tbl_fix,cols,newNames);
tbl_fix = join(tbl_fix,tbl(:,["off", cols]));
newNames = append("off_",cols);
tbl_fix = renamevars(tbl_fix,cols,newNames);

% add event info and location to saccades
cols    = ["trialNo","trialStm","trialEmo","xPixel","yPixel"];
tbl_sac = join(tbl_sac,tbl(:,["on",cols]));
tbl_sac.xPixel = tbl_sac.xPixel + ETparams.screen.resolution(1)/2 - ETparams.screen.dataCenter(1);
tbl_sac.yPixel = tbl_sac.yPixel + ETparams.screen.resolution(2)/2 - ETparams.screen.dataCenter(2);
newNames = append("on_",cols);
tbl_sac = renamevars(tbl_sac,cols,newNames);
tbl_sac = join(tbl_sac,tbl(:,["off", cols]));
tbl_sac.xPixel = tbl_sac.xPixel + ETparams.screen.resolution(1)/2 - ETparams.screen.dataCenter(1);
tbl_sac.yPixel = tbl_sac.yPixel + ETparams.screen.resolution(2)/2 - ETparams.screen.dataCenter(2);
newNames = append("off_",cols);
tbl_sac = renamevars(tbl_sac,cols,newNames);

%% save data to disk

% save data structure and classification parameters to .mat file
save([dir_path filesep subID '_prepro.mat'], 'classificationData', 'ETparams');

% save event tables for further analyses
writetable(tbl_sac, [dir_path filesep subID '_saccades.csv']);
writetable(tbl_fix, [dir_path filesep subID '_fixations.csv']);

end