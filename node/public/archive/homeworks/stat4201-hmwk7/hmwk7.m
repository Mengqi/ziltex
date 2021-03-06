CCK = [0.11, 0.11, 0.11, 0.19, 0.21, 0.22, 0.24, 0.25, 0.31, 0.18, ...
       0.27, 0.36, 0.37, 0.39, 0.47, 0.37, 0.57, 0.29, 0.30, 0.40, ...
       0.45, 0.47, 0.52, 0.57, 1.10]';
Disease = char('Controls', 'Controls', 'Controls', 'Controls',...
               'Controls', 'Controls', 'Controls', 'Controls',...
               'Controls', 'Gallstone', 'Gallstone', 'Gallstone',...
               'Gallstone', 'Gallstone', 'Gallstone', 'Gallstone',...
               'Gallstone', 'Ulcer', 'Ulcer', 'Ulcer', 'Ulcer', 'Ulcer',...
               'Ulcer', 'Ulcer', 'Ulcer');

[p,t,st] = anova1(CCK, Disease,'off');
[c,m,h,nms] = multcompare(st, 'display', 'off', 'ctype', 'bonferroni');
[nms num2cell(c)]