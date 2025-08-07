%%%transitions here
% Generate full 24x24 transition matrix based on provided transitions

% Define state names (should match the order used in the model)
state_names = {
    'p_sus','p_sus_notest','p_sus_nf','p_sus_nbt','p_sus_nct','p_sus_tc',...
    'p_sus_sae','p_sus_sae_death','p_sus_no_risk','p_ltbi','p_ltbi_notest',...
    'p_ltbi_nf','p_ltbi_nbt','p_ltbi_nct','p_ltbi_tc','p_ltbi_sae',...
    'p_ltbi_sae_death','p_ltbi_ongoing_risk','p_ltbi_no_risk','p_tb',...
    'p_tbr','p_tb_death','p_death','p_emigrate'
};

n_states = length(state_names);
param = p.cascade_of_care;

% Alias fields to match transition matrix notation

param.ATTENDCLINIC = param.att;
param.SCREEN = param.attscreen;
param.BEGINTREAT = param.begintrt;
param.TESTSN = param.snqftgit;
param.TESTSP = param.spqftgit;

T = cell(n_states);  % Transition matrix
idx = @(s) find(strcmp(state_names, s));
param = p.cascade_of_care;

% -- SUS transitions --
T{idx('p_sus_notest'), idx('p_sus')} = '1 - ( param.SCREEN)'; % not screened
T{idx('p_sus_nf'), idx('p_sus')} = '( param.SCREEN) * (1 - param.ATTENDCLINIC)'; % went to screening but not to clinic for result
T{idx('p_sus_nbt'), idx('p_sus')} = '( param.SCREEN) * (1 - param.TESTSP) * param.ATTENDCLINIC * (1 - param.BEGINTREAT)';% went to screening, got a FP result but declined treatment
T{idx('p_sus_nct'), idx('p_sus')} = '( param.SCREEN) * (1 - param.TESTSP) * param.ATTENDCLINIC * param.BEGINTREAT * (1 - param.TREATCOMPLETE - param.SAE)';% went to screening, got a FP result and were partially treated
T{idx('p_sus_tc'), idx('p_sus')} = '( param.SCREEN) * (1 - param.TESTSP) * param.ATTENDCLINIC * param.BEGINTREAT * param.TREATCOMPLETE';% went to screening, got a FP result and were  treated
T{idx('p_sus_sae'), idx('p_sus')} = '( param.SCREEN) * (1 - param.TESTSP) * param.ATTENDCLINIC * param.BEGINTREAT * param.SAE';% went to screening, got a FP result and had an adverse event

% -- SAE transitions --
T{idx('p_sus_sae_death'), idx('p_sus_sae')} = 'param.SAEMR';
T{idx('p_sus_sae_death'), idx('p_sus_sae_death')} = '1';

% -- SUS to no risk --
sus_end_states = {'p_sus_notest','p_sus_nf','p_sus_nbt','p_sus_nct','p_sus_tc','p_sus_sae','p_sus_no_risk'};
for i = 1:length(sus_end_states)
    T{idx('p_sus_no_risk'), idx(sus_end_states{i})} = 'CMP';
end

% -- LTBI transitions --
T{idx('p_ltbi_notest'), idx('p_ltbi')} = '(1 - param.SCREEN) '; % not screened
T{idx('p_ltbi_nf'), idx('p_ltbi')} = 'param.SCREEN * (1 -  param.ATTENDCLINIC)';% went to screening but not to clinic for result
T{idx('p_ltbi_nbt'), idx('p_ltbi')} = 'param.SCREEN * param.TESTSN * param.ATTENDCLINIC * (1 - param.BEGINTREAT)';% went to screening, got a TP result but declined treatment
T{idx('p_ltbi_nct'), idx('p_ltbi')} = 'param.SCREEN * param.TESTSN * param.ATTENDCLINIC * param.BEGINTREAT * (1 - param.TREATCOMPLETE - param.SAE)';% went to screening, got a TP result and were partially treated
T{idx('p_ltbi_tc'), idx('p_ltbi')} = 'param.SCREEN * param.TESTSN * param.ATTENDCLINIC * param.BEGINTREAT * param.TREATCOMPLETE';% went to screening, got a TP result and were  treated
T{idx('p_ltbi_sae'), idx('p_ltbi')} = 'param.SCREEN * param.TESTSN * param.ATTENDCLINIC * param.BEGINTREAT * param.SAE';% went to screening, got a TP result and had an adverse event
T{idx('p_ltbi_sae_death'), idx('p_ltbi_sae')} = 'param.SAEMR';
T{idx('p_ltbi_sae_death'), idx('p_ltbi_sae_death')} = '1';

ltbi_paths = {'p_ltbi_notest','p_ltbi_nf','p_ltbi_nbt','p_ltbi_nct','p_ltbi_tc','p_ltbi_sae','p_ltbi_ongoing_risk'};
for i = 1:length(ltbi_paths)
    T{idx('p_ltbi_ongoing_risk'), idx(ltbi_paths{i})} = 'CMP';
end

T{idx('p_ltbi_no_risk'), idx('p_ltbi_nct')} = 'param.PART_TREAT_EFFICACY';
T{idx('p_ltbi_no_risk'), idx('p_ltbi_tc')} = 'param.FULL_TREAT_EFFICACY';
T{idx('p_ltbi_no_risk'), idx('p_ltbi_no_risk')} = 'CMP';
T{idx('p_ltbi_ongoing_risk'), idx('p_ltbi_nct')} = '(1-param.PART_TREAT_EFFICACY)';
T{idx('p_ltbi_ongoing_risk'), idx('p_ltbi_tc')} = '(1-param.FULL_TREAT_EFFICACY)';

% -- Reactivation to TB --
T{idx('p_tb'), idx('p_ltbi')} = 0;
T{idx('p_tb'), idx('p_ltbi_notest')} = 0;
T{idx('p_tb'), idx('p_ltbi_nf')} = 0;
T{idx('p_tb'), idx('p_ltbi_nbt')} = 0;
T{idx('p_tb'), idx('p_ltbi_nct')} = 0;
T{idx('p_tb'), idx('p_ltbi_tc')} = 0;
T{idx('p_tb'), idx('p_ltbi_sae')} = 0';
T{idx('p_tb'), idx('p_ltbi_ongoing_risk')} = 'param.RR * param.RRADJUST';% Reactivation to TB occurs only from ongoing risk group


% -- TB to TBR and death
T{idx('p_tbr'), idx('p_tb')} = 'CMP';
T{idx('p_tbr'), idx('p_tbr')} = 'CMP';
T{idx('p_tb_death'), idx('p_tb')} = 'param.TBMR';
T{idx('p_tb_death'), idx('p_tb_death')} = '1';

% -- Background mortality
for i = 1:length(state_names)
    if ~any(strcmp(state_names{i}, {'p_tb','p_tbr','p_tb_death','p_death','p_emigrate'}))
        T{idx('p_death'), idx(state_names{i})} = 'param.MR';
    end
end
T{idx('p_death'), idx('p_death')} = '1';

% -- Emigration
for i = 1:length(state_names)
    if ~strcmp(state_names{i}, 'p_emigrate') && ~strcmp(state_names{i}, 'p_death')
        T{idx('p_emigrate'), idx(state_names{i})} = 'param.EMIGRATE';
    end
end
T{idx('p_emigrate'), idx('p_emigrate')} = '1';

% Optional: Save to .mat or CSV
% save('transition_matrix_expressions.mat', 'T', 'state_names');
% writecell(T, 'transition_matrix_expressions.csv');

% Fill in empty cells with '0'
for i = 1:n_states
    for j = 1:n_states
        if isempty(T{i, j})
            T{i, j} = '0';
        end
    end
end

% Create header row (top and left)
header = ['FROM\TO', state_names];
csv_out = [header; [state_names', T]];

% Save to CSV
writecell(csv_out, 'transition_matrix_expressions.csv');



