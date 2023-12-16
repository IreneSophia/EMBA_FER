function FER_mc(opt, xCenter, yCenter, window, black, grey)

gap = 50;
xsz = 200;
ysz = 100;

rects = {[xCenter - 2*xsz - 1.5*gap yCenter - 2*ysz/3 xCenter - 1*xsz - 1.5*gap yCenter + 1*ysz/3];...
    [xCenter - 1*xsz - 0.5*gap yCenter - 2*ysz/3 xCenter - 0*xsz - 0.5*gap yCenter + 1*ysz/3];...
    [xCenter + 0*xsz + 0.5*gap yCenter - 2*ysz/3 xCenter + 1*xsz + 0.5*gap yCenter + 1*ysz/3];...
    [xCenter + 1*xsz + 1.5*gap yCenter - 2*ysz/3 xCenter + 2*xsz + 1.5*gap yCenter + 1*ysz/3]};

% Choose the font.
Screen('TextFont', window, 'Arial');
Screen('TextSize', window, 50);

% Draw some grey boxes.
for i = 1:length(rects)
    Screen('FillRect', window, grey, rects{i});
end

Screen('FrameRect', window, [0 0 1], rects{opt} + [-5 -5 5 5], 10);

% Draw some text in the window.
DrawFormattedText(window,... % the first input to drawing functions is almost always the window variable
    'Angst',... % the second input to DrawFormattedText() is the text we want to draw
    xCenter - 450,... % then the x position of the start of the text (in pixels from the left)
    yCenter,... % then the y position of the start of the text (in pixels from the top)
    black); % then the colour of the text (as an [R G B] vector)
DrawFormattedText(window, 'Ärger', xCenter - 200, yCenter, black); 
DrawFormattedText(window, 'Freude', xCenter + 50, yCenter, black); 
DrawFormattedText(window, 'Trauer', xCenter + 300, yCenter, black); 

% Things that we draw into the window will only actually be displayed
% when our script reaches a 'Flip' command.
Screen('Flip',window);

end