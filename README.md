# Ketamin
*Ketamin* is a simplistic embedded scripting language with a focus on ease of use

```
function greet(person) {
    print(
        "Hallo,",
        if person.gender == "male" {
            "Herr";
        } else if person.gender == "female" {
            "Frau";
        },
        person.last_name + "!"
    );
}

var myself = {
    gender: "male",
    first_name: "Moritz",
    last_name: "Bischof"
};
greet(myself);
```
`Hallo, Herr Bischof!`

**This project is not meant for production use yet!**