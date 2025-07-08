from metaflow import FlowSpec, step, current


class SimpleFlow(FlowSpec):

    @step
    def start(self):
        print(f"Starting simple flow with run_id: {current.run_id}")
        self.next(self.do_stuff)

    @step
    def do_stuff(self):
        print("Doing stuff...")
        self.next(self.end)

    @step
    def end(self):
        print("Simple flow completed!")
        print(f"Run ID: {current.run_id}")

if __name__ == '__main__':
    SimpleFlow()
