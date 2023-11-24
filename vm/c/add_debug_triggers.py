import re

# Define the pattern to match the start of rules in the grammar
start_rule_pattern = re.compile(r"(\w+)\s*<-\s*(.+)")

def add_debug_actions(grammar):
    # Split the grammar into lines
    lines = grammar.split('\n')

    # Buffer to hold a rule's lines
    rule_lines = []
    modified_lines = []

    # Process each line
    for line in lines:
        # Check if the line starts a new rule or is a continuation of the current rule
        if start_rule_pattern.match(line):
            # If there's a current rule in the buffer, process it
            if rule_lines:
                modified_lines.extend(process_rule(rule_lines))
                rule_lines = []
            rule_lines.append(line)
        elif rule_lines and not line.strip().startswith('#'):  # Assuming '#' starts a comment
            # Continuation of the current rule
            rule_lines.append(line)
        else:
            # No current rule, so just copy the line
            modified_lines.append(line)

    # Process any remaining rule in the buffer
    if rule_lines:
        modified_lines.extend(process_rule(rule_lines))

    # Join the modified lines back into a single string
    return '\n'.join(modified_lines)

def process_rule(rule_lines):
    # Combine the lines for the current rule
    rule_text = ' '.join(rule_lines)
    match = start_rule_pattern.match(rule_text)
    if match:
        # Extract the rule name
        rule_name = match.group(1)
        # Create a debug action
        debug_action = ' { fprintf(stderr, "Matched {}\\n"); }'.format(rule_name)
        # Append the debug action to the last line of the rule
        rule_lines[-1] = rule_lines[-1] + debug_action
    return rule_lines

# Read the original grammar from a file
with open('satie2.peg', 'r') as file:
    original_grammar = file.read()

# Add debug actions to the grammar
modified_grammar = add_debug_actions(original_grammar)

# Write the modified grammar to a new file
with open('modified_grammar.peg', 'w') as file:
    file.write(modified_grammar)

print("Modified grammar written to modified_grammar.peg")
