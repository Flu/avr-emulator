import sys

def convert_file(filename):
    lines = []
    with open(filename, "r") as f:
        while line := f.readline():
            lines.append('"' + line.strip() + r'\n\t"')

    return lines

def main():
    filename = sys.argv[1]
    converted_file = convert_file(filename)
    for line in converted_file:
        print(line)

if __name__ == "__main__":
    main()