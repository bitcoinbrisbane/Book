# Game Specification

Games are specified declaratively, describing components, setup, rounds, and victory conditions.

## Cut for High Card

We present the complete specification for the simplest non-trivial card game:

```yaml
game: cut_for_high_card
version: 1.0

components:
  deck: standard_52

ranking:
  type: high_card
  ace: high
  suits_break_ties: false

players:
  count: 2..8

setup:
  - shuffle: deck

round:
  name: cut

  actions:
    - each_player:
        - draw: 1 from deck -> hand
        - reveal: hand

  resolve:
    compare: players.hand using ranking
    winner: highest

  on_tie:
    - reshuffle: revealed cards into deck
    - repeat: round

victory:
  condition: win_round
  award: pot
```

## Compilation Target

The CardLang compiler transforms this specification into executable code with the following properties:

1. **State Machine**: The game becomes a finite state machine with well-defined transitions.
2. **Verified Operations**: `shuffle` compiles to Fisher-Yates; `compare` uses the specified ranking.
3. **Determinism**: Given initial state and seed, all outcomes are reproducible.
4. **Audit Trail**: Every state transition is logged for verification.

## Extended Example: Texas Hold'em

To demonstrate the expressiveness of CardLang, we present the specification for Texas Hold'em poker:

```yaml
game: texas_holdem
version: 1.0

components:
  deck: standard_52

ranking:
  type: poker_hand
  ace: high_and_low  # for straights

players:
  count: 2..10

positions:
  - dealer: rotates
  - small_blind: dealer + 1
  - big_blind: dealer + 2

setup:
  - shuffle: deck
  - post: small_blind -> pot
  - post: big_blind -> pot
  - deal: 2 to each_player -> hand (hidden)

rounds:
  - name: preflop
    betting: standard

  - name: flop
    actions:
      - burn: 1 from deck
      - deal: 3 from deck -> community (revealed)
    betting: standard

  - name: turn
    actions:
      - burn: 1 from deck
      - deal: 1 from deck -> community (revealed)
    betting: standard

  - name: river
    actions:
      - burn: 1 from deck
      - deal: 1 from deck -> community (revealed)
    betting: standard

showdown:
  evaluate: best_5_of_7(hand + community) using ranking
  winner: highest
  award: pot

  on_tie:
    split: pot among tied_players
```

## Specification Components

### Components Section

Defines the deck type and any special cards:

```yaml
components:
  deck: standard_52
  # Or custom decks:
  # deck: euchre
  # deck: pinochle
```

### Players Section

Defines player count and positions:

```yaml
players:
  count: 2..10  # Range or fixed number

positions:
  - dealer: rotates
  - small_blind: dealer + 1
```

### Setup Section

Initialization actions before gameplay begins:

```yaml
setup:
  - shuffle: deck
  - deal: 5 to each_player from deck
  - deal: 1 to community from deck (revealed)
```

### Rounds Section

Defines the phases of play:

```yaml
rounds:
  - name: draw
    actions:
      - each_player: may discard up to 3
      - each_player: draw discarded_count from deck
```

### Victory Section

Win conditions and rewards:

```yaml
victory:
  condition: highest_hand
  award: pot

  on_tie:
    split: pot among winners
```
