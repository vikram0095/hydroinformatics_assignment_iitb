'clear all
close all
[A,B]=xlsread('IMD_Subdiv_Rain_Data_for_Assignment_1.xlsx');
data= A(:,3:end);
% extracting even data set
data1=data(2:2:end,:);
data2=data(1:2:end,:);
%West Rajasthan and Telangana
west_raj=data1(2:3:end,:);
telangana=data1(3:3:end,:);

scatter(west_raj(:,1),west_raj(:,5),'gd')
hold on
scatter(telangana(:,1),telangana(:,5),'bx')
xlabel('Year');
ylabel('Rainfall (mm)')
title('Rainfall in June-September')
legend('West Rajasthan','Telangana','Location','northwest')
axis([2000 2011 0 1200])
figure(2)

pie(telangana(5,3:end),{'Jan-Feb','Mar-May','Jun-Sep','Oct-Dec'})
title('Annual Distribution of rainfall in Telangana for year 2005')

figure(3)
indi = shaperead('./shape_file/india.shp');
mapshow(indi, 'FaceColor','red')






