test__users = [ "Agata", "Szymon", "Borsuk", "Dzik"]
test__sentences = ["Co się dzieje w lesie po zmroku?", "Czy należy się bać dzika.", "Czerwcowe motyle"]

test__entries = [
    [
        test__users[1],
        "Analogiczna misja tego roku?",
        ["analogia", "misja"]
    ],
    [   
        test__users[0],
        "Jaka klawiature kupic?",
        ["klawiatura", "kupno"],
    ],
    [
        test__users[0],
        "Piekne drzewa owocowe naszły na się...",
        ["wazne", "pieknedrzewa", "owadynocne"],
    ],
    [
        test__users[0],
        "Czym sa drzewa? Pieknem?",
        ["wazne", "pieknedrzewa", "tenjestinny"]
    ]
]

test__userFromEntries = []
for entry in test__entries:
    user = entry[0]
    if user not in test__userFromEntries:
        test__userFromEntries.append(entry[0])

test__sentenes__agata_wazne = [
    "Piekne drzewa owocowe naszły na się...",
    "Czym sa drzewa? Pieknem?"
]           
            