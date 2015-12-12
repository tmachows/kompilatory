
class Memory:

    def __init__(self, name): # memory name
        self.name = name
        self.memory = {}

    def has_key(self, name):  # variable name
        return self.memory.has_key(name)

    def get(self, name):         # gets from memory current value of variable <name>
        if self.has_key(name):
            return self.memory[name]
        return None

    def put(self, name, value):  # puts into memory current value of variable <name>
        self.memory[name] = value

        
class MemoryStack:
                                                                             
    def __init__(self, memory=None): # initialize memory stack with memory <memory>
        self.stack = []
        if memory is not None:
            self.stack.append(memory)
        else:
            self.stack.append(Memory("global"))

    def get(self, name):             # gets from memory stack current value of variable <name>
        ind = range(1, len(self.stack) + 1)
        for i in ind:
            if self.stack[-i].has_key(name):
                return self.stack[-i].get(name)
        return None

    def insert(self, name, value): # inserts into memory stack variable <name> with value <value>
        self.stack[-1].put(name, value)

    def set(self, name, value): # sets variable <name> to value <value>
        ind = range(1, len(self.stack) + 1)
        for i in ind:
            if self.stack[-i].has_key(name):
                self.stack[-i].put(name, value)
                break
        
    def push(self, memory): # pushes memory <memory> onto the stack
        self.stack.append(memory)

    def pop(self):          # pops the top memory from the stack
        return self.stack.pop()