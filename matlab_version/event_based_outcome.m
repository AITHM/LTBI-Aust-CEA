% run a sequence for a person

a=1;
person.age=60;
person.risk_sae = .1; %replace this with a function
person.attend_screen = param.attscreen;
person.attend_clinic = param.att;

person.state = cell(90, 1);
person.state{1} ='ltbi';
regimen_sens = param.snqftgit;

if contains(person.state{a}, 'ltbi')
    p.notest = 1 - person.attend_screen;
    p.testpos = person.attend_screen * regimen_sens;
    p.testneg = person.attend_screen * (1 - regimen_sens);



events = {'noTest', 'testPositive', 'testNegative'};
weights = [p.notest, p.testpos, p.testneg];
chosenEvent = datasample(events, 1, 'Weights', weights);

a=a+1;
if contains(chosenEvent{1},'noTest'),
    % Handle the case where no test is conducted
    disp('No test conducted for the person.');
    person.state{a} = 'LTBI no test';

elseif contains(chosenEvent{1},'testPositive')
    % Handle the case where no test is conducted

    disp('test was a true positive.');
    person.state{a} = 'LTBI test positive';
    events = {'no follow-up', 'follow-up'};
    a=a+1;
    p.no_clinic_att = 1 - person.attend_clinic;
    p.clinic_att = person.attend_clinic;
    weights = [p.no_clinic_att, p.clinic_att]
    chosenEvent2 = datasample(events, 1, 'Weights', weights);
    if contains(chosenEvent2{1},'no follow-up')
        % Handle the case where no test is conducted

        disp('test was a true positive, but there was no follow-up.');
        person.state{a} = 'LTBI TP no clinical follow up';

    else
        % Handle the case where follow-up is conducted
        disp('test was a true positive, and there was follow-up.');
        person.state{a} = 'LTBI TP follow-up';
    end
elseif contains(chosenEvent{1},'testNegative')
    % Handle the case where no test is conducted

    disp('test was a false negative.');
    person.state{a} = 'LTBI false negative';
    events = {'no follow-up', 'follow-up'};
    a=a+1;
    p.no_clinic_att = 1 - person.attend_clinic;
    p.clinic_att = person.attend_clinic;
    weights = [p.no_clinic_att, p.clinic_att];
    chosenEvent2 = datasample(events, 1, 'Weights', weights);
    if contains(chosenEvent2{1},'no follow-up')
        % Handle the case where no test is conducted

        disp('test was a false negative, and there was no follow-up.');
        person.state{a} = 'LTBI FN no clinical followup';
    else
        % Handle the case where follow-up is conducted
        disp('test was a false positive, and there was follow-up.');
        person.state{a} = 'LTBI FN follow-up';
    end
else disp('error in event 1')


end




end


