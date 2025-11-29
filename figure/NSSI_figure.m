categories = {'Age', 'Grade', 'Gender', 'Depression', 'Anxiety', 'D1', 'D2'};
values = [0.82, 1.74, 0.72, 1.12, 1.04, 0.22, 1.54];

f = figure('Color', 'white', 'Position', [100 100 800 800]);
ax = axes('Parent', f);
hold(ax, 'on');
axis(ax, 'equal');
axis(ax, 'off');

numPoints = length(categories);
maxR = 2.5;
gridLevels = 0:0.5:maxR;

angles = -pi/2 - (0:numPoints-1)*(2*pi/numPoints);

for r = gridLevels
    x = r * cos(angles);
    y = r * sin(angles);
    plot([x, x(1)], [y, y(1)], 'Color', [0.7 0.7 0.7], 'LineWidth', 0.5);
end

center = [0, 0];
for i = 1:numPoints
    x = maxR * cos(angles(i));
    y = maxR * sin(angles(i));
    plot([center(1), x], [center(1), y], 'Color', [0.7 0.7 0.7], 'LineWidth', 0.5);
end

x_data = values .* cos(angles);
y_data = values .* sin(angles);

h_fill = fill([x_data, x_data(1)], [y_data, y_data(1)], [0.2 0.6 0.8], ...
    'FaceAlpha', 0.5, ...
    'EdgeColor', [0.2 0.4 0.8], ...
    'LineWidth', 2, ...
    'DisplayName', 'OR');

scatter(x_data, y_data, 60, 'filled', ...
    'MarkerFaceColor', [0.2 0.4 0.8], ...
    'MarkerEdgeColor', 'k', ...
    'LineWidth', 1.5);

labelPositions = 1.12 * maxR * [cos(angles); sin(angles)];
for i = 1:numPoints
    text(labelPositions(1, i), labelPositions(2, i), categories{i}, ...
        'FontSize', 12, ...
        'FontWeight', 'bold', ...
        'HorizontalAlignment', 'center', ...
        'VerticalAlignment', 'middle');
end

rLabels = {'0', '0.5', '1.0', '1.5', '2.0', '2.5'};
rLabelAngles = pi/2;

for i = 1:length(gridLevels)
    r = gridLevels(i);
    x = r * cos(rLabelAngles);
    y = r * sin(rLabelAngles);
    text(x, y, rLabels{i}, ...
        'FontSize', 10, ...
        'FontWeight', 'bold', ...
        'HorizontalAlignment', 'center', ...
        'VerticalAlignment', 'bottom');
end

text(0, 1.2 * maxR, '(b) NSSI', ...
    'FontSize', 16, ...
    'FontWeight', 'bold', ...
    'HorizontalAlignment', 'center');

lgd = legend(h_fill, 'Location', 'southoutside');
lgd.FontSize = 12;
lgd.FontWeight = 'bold';

axis([-maxR*1.3, maxR*1.3, -maxR*1.3, maxR*1.3]);

exportgraphics(f, 'radar_chart_NSSI.tif', 'Resolution', 1000, 'ContentType', 'image');
disp('High-resolution TIFF radar chart saved as radar_chart_NSSI.tif (1000 DPI)');
