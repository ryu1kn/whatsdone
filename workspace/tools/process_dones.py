import json
import sys
from pathlib import Path

def main():
    # The JSON data is passed as a command-line argument
    if len(sys.argv) < 2:
        print("Usage: python process_dones.py <json_data>")
        sys.exit(1)

    json_data = sys.argv[1]
    dones = json.loads(json_data)

    script_dir = Path(__file__).resolve().parent
    # Navigate up to the project root and then to the target file
    project_root = script_dir.parent.parent
    output_file_path = project_root / 'tools' / 'llm-eval' / 'data' / 'dataset.jsonl'

    with open(output_file_path, 'w', encoding='utf-8') as f:
        for done in dones:
            if 'doneThing' in done:
                record = {
                    "text": done["doneThing"],
                    "expected_topic": ""
                }
                f.write(json.dumps(record, ensure_ascii=False) + '\n')

if __name__ == '__main__':
    main()