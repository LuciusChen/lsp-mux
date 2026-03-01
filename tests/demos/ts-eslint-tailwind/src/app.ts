type User = {
  id: number;
  name: string;
};

const unusedUser: User = { id: 1, name: "Lucius" };

function greet(name: string): string {
  console.log("Hello", name);
  return `Hello, ${name}`;
}

greet(43);
