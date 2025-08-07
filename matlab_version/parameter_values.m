% Load Excel file
excelFile = 'parameters.xlsx';
[~, sheetNames] = xlsfinfo(excelFile);

% Initialize output structure
p = struct();

% Loop through each sheet
% Loop through each sheet
for s = 1:length(sheetNames)
    sheet = sheetNames{s};
    sheet_name = matlab.lang.makeValidName(sheet);

    % If it's a matrix-style sheet, read it as a table
    if any(strcmpi(sheet, {'mortality', 'tb_mortality', 'sae_rate', 'sae_mortality', 'rradjrates'}))
        p.(sheet_name) = readtable(excelFile, 'Sheet', sheet);
    
    % Otherwise parse it as key-value pair format
    else
        [~, ~, raw] = xlsread(excelFile, sheet);

        if size(raw, 2) < 2
            warning('Skipping sheet %s: less than 2 columns.', sheet);
            continue;
        end

        raw = raw(~cellfun(@(x) all(isnan(x) | strcmp(x, '')), raw(:,1)), :);
        sheet_struct = struct();

        for i = 2:size(raw, 1)  % Skip header
            key = matlab.lang.makeValidName(raw{i, 1});
            val = raw{i, 2};
            sheet_struct.(key) = val;
        end

        p.(sheet_name) = sheet_struct;
    end
end


% ✅ Example usage
disp(p.switches.onshore)
disp(p.costs.c_liver)
disp(p.utilities.uhealthy)

%%
% parameter_values.m
% Assumes that the `p` struct is already created from reading parameters.xlsx

% === Assign Switches ===
onshore         = p.switches.onshore;
emigration      = p.switches.emigration;
payerperspect   = p.switches.payerperspect;
disc            = p.switches.disc;
startyear       = p.switches.startyear;
totalcycles     = p.switches.totalcycles;
finalyear       = startyear + totalcycles;
kill_off_above  = p.switches.kill_off_above;
migrant_inflow  = p.switches.migrant_inflow_size;
finalinflow     = p.switches.finalinflow;

testlist        = strsplit(p.switches.test, ',');
treatmentlist   = strsplit(p.switches.treatment, ',');

% === Utilities ===
uhealthy    = p.utilities.uhealthy;
ultbi3HP    = p.utilities.ultbi3HP;
ultbi4R     = p.utilities.ultbi4R;
ultbi6H     = p.utilities.ultbi6H;
ultbi9H     = p.utilities.ultbi9H;

% === Derived Utilities ===
part_utility_dec = 0.5;
ultbipart3HP = uhealthy - ((uhealthy - ultbi3HP) * part_utility_dec);
ultbipart4R  = uhealthy - ((uhealthy - ultbi4R) * part_utility_dec);
ultbipart6H  = uhealthy - ((uhealthy - ultbi6H) * part_utility_dec);
ultbipart9H  = uhealthy - ((uhealthy - ultbi9H) * part_utility_dec);

% === Partial Cost Parameters ===
part_appt = 2;
part_med  = 3;

% === Cost Calculations ===
prop_nonvr = p.cascade_of_care.proportion_nonvr;
c_gp_first  = p.costs.c_gp_c_vr   * (1 - prop_nonvr) + p.costs.c_gp_c_nonvr   * prop_nonvr;
c_gp_review = p.costs.c_gp_b_vr   * (1 - prop_nonvr) + p.costs.c_gp_b_nonvr   * prop_nonvr;
c_mcs       = p.costs.c_mcs;
c_cxr       = p.costs.c_cxr;

% === Specialist Costs ===
c_spec_first  = p.costs.c_spec_first;
c_spec_review = p.costs.c_spec_review;
prop_spec     = p.cascade_of_care.prop_spec;
chance_mcs    = p.cascade_of_care.chance_of_needing_mcs;

% === Determine Attendance Cost ===
if onshore == 0
    cattend = c_gp_first + c_mcs * chance_mcs + c_cxr;
else
    cattend = ((c_gp_review + c_mcs * chance_mcs + c_cxr) * (1 - prop_spec)) + ...
              ((c_spec_first + c_mcs * chance_mcs + c_cxr) * prop_spec);
    c_spec_first = c_spec_review;  % Override logic from R
end

% === Treatment Loop Setup ===
regimens = {'3HP', '4R', '6H', '9H'};
treatment = struct();

for r = 1:length(regimens)
    name = regimens{r};

    % Build dynamic field names
    n_appt = p.cascade_of_care.(['num_appt' name]);
    med    = p.costs.(['cmed' name]);

    % Cost formulas
    appt      = n_appt * c_gp_review + (ismember(name, {'3HP','6H','9H'}) * p.costs.c_liver);
    spec_appt = c_spec_first + (n_appt - 1) * c_spec_review + (ismember(name, {'3HP','6H','9H'}) * p.costs.c_liver);

    % Assign to struct
    treatment.(['ctreat' name])        = appt + med;
    treatment.(['cparttreat' name])    = appt / part_appt + med / part_med;
    treatment.(['ctreatspec' name])    = spec_appt + med;
    treatment.(['cparttreatspec' name]) = spec_appt / part_appt + med / part_med;
end

% === Example output for verification ===
disp("=== Example Parameters Loaded ===")
fprintf('Start year: %d\n', startyear);
fprintf('Disc rate: %.3f\n', disc);
fprintf('Attendance cost: %.2f\n', cattend);
fprintf('Treatment cost (3HP): %.2f\n', treatment.ctreat3HP);
