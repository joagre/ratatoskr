import sys

def remove_nested_braces(content):
    stack = []
    new_content = []

    for char in content:
        if char == '{':
            stack.append(char)
        elif char == '}':
            if stack:
                stack.pop()
        elif not stack:
            new_content.append(char)

    return ''.join(new_content)

def post_process(content):
    lines = content.splitlines()
    processed_lines = []

    for line in lines:
        if line.startswith('%%'):
            break
        if not line.startswith('%'):
            processed_lines.append(line)

    return '\n'.join(processed_lines).strip() + '\n'

def main():
    content = sys.stdin.read()
    brace_removed_content = remove_nested_braces(content)
    final_content = post_process(brace_removed_content)
    sys.stdout.write(final_content)

if __name__ == "__main__":
    main()
