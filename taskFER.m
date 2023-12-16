% This function presents the facial emotion recognition (FER) task from the
% EMOPRED project. (c) Irene Sophia Plank, irene.plank@med.uni-muenchen.de
% Per trial 300 pictures are presented, each picture with a new refresh of
% the screen so that the pictures seem like a video. The video shows an
% emotionally neutral face gradually and continually take on an emotion.
% The video can be stopped with key_stop as soon as the emotion has been
% recognised. Then, participants can choose the corresponding emotion from
% four options by using the key_left and key_right to select an option that
% they confirm by using the key_choose. There are 64 trials.
% This function takes two inputs: 
% * subID        :  a string array with the subjects PID
% * eyeTracking  :  0 or 1 to choose eye tracking or not
% The function needs Psychtoolbox to function properly. If eye tracking has
% been chosen, then a LiveTracking Eye Tracker has to be connected and the
% LiveTrackToolbox for MATLAB has to be installed. The function is meant to
% be used without an external monitor. Using a second monitor can seriously
% mess up the timing. 
% The function continually saves behavioural data as well as eye tracking
% data if that option is chosen. Both files will be placed in the "Data"
% folder. 
function taskFER

% Get all relevant inputs. 
inVar = inputdlg({'Enter PID: ', 'Eye Tracking? 0 = no, 1 = yes'}, 'Input variables', [1 45]);
subID = convertCharsToStrings(inVar{1});
eyeTracking = str2double(inVar{2});

% Get the path of this function. 
path_src = fileparts(mfilename("fullpath")); 
idx  = strfind(path_src, filesep);
path_dat = path_src(1:(idx(end)-1));

% Clear the screen
sca;
close all;

% Initialise some settings
fx    = 40;     % size of the size of the arms of our fixation cross
fxdur = 0.1;   % duration of the fixation cross in seconds
maxrs = 8;      % maximum response time 

% Which keys are used for what
key_stop   = '4';
key_left   = '4';
key_right  = '6';
key_choose = '8';

% Open a csv file into which you can write information
fid = fopen(path_dat + "\Data\FER-BV-" + subID + "_" + datestr(datetime(),'yyyymmdd-HHMM') + ".csv", 'w');
fprintf(fid, 'subID,trl,video,emo,disc,opt,lastkey\n');

% Add eye tracking stuff, if eyeTracking is set to 1. 
if eyeTracking
    % Initialise LiveTrack
    crsLiveTrackInit;
    % Open a data file to write the data to.
    crsLiveTrackSetDataFilename(char(path_dat + "\Data\FER-ET-" + subID + "_" + datestr(datetime(),'yyyymmdd-HHMM') + ".csv"));
end

% Here we call some default settings for setting up Psychtoolbox
PsychDefaultSetup(2);

% This use of the subfunction 'UnifyKeyNames' within KbName()
% sets the names of the keys to be the same across operating systems
% (This is useful for making our experiment compatible across computers):
KbName('UnifyKeyNames');

% Create stimulus order by starting of with list of videos and assuming
% that their order violates our conditions.
load([path_src '\vid_list.mat'],'vids')
cond = 0;
% As long as the conditions are violated...
while ~cond
    % ... we first randomise their order and take a look at the emotions.
    vids = Shuffle(vids,2);
    emos = horzcat(vids{:,2});
    % If there are no emotions that are repeated three or more times in a
    % row...
    if isempty([strfind(emos,[1 1 1]),...
            strfind(emos,[2 2 2]),...
            strfind(emos,[3 3 3]),...
            strfind(emos,[4 4 4])])
        % ... our conditions are fulfilled
        cond = 1;
    end
end

% Get number of trials. 
ntrials = length(vids);

% Get the screen numbers. This gives us a number for each of the screens
% attached to our computer.
% For help see: Screen Screens?
screens = Screen('Screens');

% Adjust tests of the system:
Screen('Preference','SkipSyncTests', 0);
Screen('Preference','SyncTestSettings', 0.0025);

% Draw we select the maximum of these numbers. So in a situation where we
% have two screens attached to our monitor we will draw to the external
% screen. When only one screen is attached to the monitor we will draw to
% this.
% For help see: help max
screenNumber = max(screens);

% Define grey and black.
grey  = [0.67 0.67 0.67];
black = BlackIndex(screenNumber);

% And the function ListenChar(), with the single number input 2,
% stops the keyboard from sending text to the Matlab window.
ListenChar(2);
% To switch the keyboard input back on later we will use ListenChar(1).

% As before we start a 'try' block, in which we watch out for errors.
try

    % Open an on screen window and color it black
    % For help see: Screen OpenWindow?
    [window, windowRect] = PsychImaging('OpenWindow', screenNumber, black);

    % Hide the mouse cursor
    HideCursor(window);
    
    % Get the centre coordinate of the window in pixels
    % For help see: help RectCenter
    [xCenter, yCenter] = RectCenter(windowRect);

    % Query the frame duration
    ifi = Screen('GetFlipInterval', window);

    % Set up alpha-blending for smooth (anti-aliased) lines
    Screen('BlendFunction', window, 'GL_SRC_ALPHA', 'GL_ONE_MINUS_SRC_ALPHA');

    % Here we set the size of the arms of our fixation cross
    fixCrossDimPix = fx;
    
    % Now we set the coordinates (these are all relative to zero we will let
    % the drawing routine center the cross in the center of our monitor for us)
    xCoords = [-fixCrossDimPix fixCrossDimPix 0 0];
    yCoords = [0 0 -fixCrossDimPix fixCrossDimPix];
    allCoords = [xCoords; yCoords];
    
    % Set the line width for our fixation cross
    lineWidthPix = round(fx/10);

    if eyeTracking
        % Start streaming calibrated results
        crsLiveTrackSetResultsTypeCalibrated;
        % Start tracking
        crsLiveTrackStartTracking;
    end
    
    % Go through the trials
    for j = 1:ntrials
    
        % Draw the fixation cross in grey, set it to the center of our screen and
        % set good quality antialiasing
        Screen('DrawLines', window, allCoords,...
            lineWidthPix, grey, [xCenter yCenter], 2);

        % Flip the fixation cross to the screen
        Screen('Flip', window);
        WaitSecs(fxdur-ifi);

        % Determine which video is loaded. 
        video = vids{j,1};
        emo = vids{j,2};

        % Load all pics making up this video.
        pic_files = dir([path_src '\Stimuli\' video '*']);
        pics = nan(length(pic_files),1);
        for i = 1:length(pic_files)
            pic = imread([pic_files(i).folder filesep pic_files(i).name]);
            pics(i) = Screen('MakeTexture', window, pic);
        end
        pic_width  = size(pic,2);
        pic_height = size(pic,1);
        
        % Draw the pictures to the screen. 
        for i = 1:length(pics)
            Screen('DrawTexture', window, pics(i), [], ... % we want the images to be twice their normal size
                [xCenter-pic_width yCenter-pic_height xCenter+pic_width yCenter+pic_height]);
            % Flip the pic to the screen
            Screen('Flip', window);
            if eyeTracking
                % Add a comment/trigger to the eye tracking data. 
                crsLiveTrackSetDataComment(sprintf('pic_%i_%i_%i',...
                    j,i,emo));
            end
            Screen('Close',pics(i))
            % check if a Key was pressed
            if i < 25
                pressed = 0;
                key = '';
            else
                [pressed,t_stop,keys] = KbCheck;
                key = KbName(keys);
            end
            if isa(key, 'cell')
                key = key{1};
            end
            % if the right key was pressed, stop the presentation
            if pressed && strcmp(key,key_stop)
                break
            elseif pressed && strcmp(key,'ESCAPE')
                error('Experiment aborted.')
            end
        end
        disc = i/length(pics); % discrimination sensitivity - seen pics / number of pics

        % Wait so that the key press doesn't carry over. 
        WaitSecs(0.2);

        % We are going to stay in the options menu until the upwards arrow
        % has been pressed. We start with no key and a random option.
        key = '';
        opt = randi(4);
        while ~strcmp(key,key_choose) && (GetSecs - t_stop < maxrs)
            % Wait a bit to make sure no 'rogue' key strokes are logged.
            WaitSecs(0.1);
            % This function actually draws the options to the screen with a
            % blue rectangle around the chosen option. 
            FER_mc(opt, xCenter, yCenter, window, black, grey);
            % Check if and which key was pressed.
            [~,~,keys] = KbCheck;
            key = KbName(keys);
            if iscell(key)
                key = key{1};
            end
            % If the right arrow was pressed and it is possible go right.
            if strcmp(key, key_right) && opt < 4
                opt = opt + 1;
            % If the left arrow was pressed and it is possible go left.
            elseif strcmp(key, key_left) && opt > 1
                opt = opt - 1;
            % If escape is pressed, abort the experiment.
            elseif strcmp(key, 'ESCAPE')
                error('Experiment aborted.')
            end
        end

        % Log all the information for this trial
        fprintf(fid, '%s,%i,%s,%i,%.4f,%i,%s\n',subID,j,video,emo,disc,opt,key);

    end
    
    % If we encounter an error...
catch my_error

    % Close all open objects
    Screen('Close');

    % Show the mouse cursor
    if exist('window', 'var')
        ShowCursor(window);
    end
    
    % Clear the screen (so we can see what we are doing).
    sca;
    
    % In addition, re-enable keyboard input (so we can type).
    ListenChar(1);

    % Close the open csv file 
    fclose(fid);

    % Stop eye tracking. 
    if eyeTracking
        crsLiveTrackStopTracking;
        crsLiveTrackCloseDataFile;
        crsLiveTrackClose;
    end
    
    % Tell us what the error was.
    rethrow(my_error)
    
end

% At the end, clear the screen and re-enable the keyboard.
Screen('Close');
ShowCursor(window);
sca;
ListenChar(1);
fclose(fid);
if eyeTracking
    crsLiveTrackStopTracking;
    crsLiveTrackCloseDataFile;
    crsLiveTrackClose;
end

end
