% #room_state.players saved as: {player_id, player_pid, ship_id}
-record(room_state, {players = [], id, life, asteroids_position, ships_position = [[]]}).
