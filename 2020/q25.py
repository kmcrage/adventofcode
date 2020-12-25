#!/usr/bin/python3
cpk = 12090988
dpk = 240583
# cpk = 5764801
# dpk = 17807724
base = 20201227


def transform(subject, loop, bs=20201227):
    num = 1
    for _ in range(loop):
        num = (num * subject) % bs
    return num


#
# def handshake(card_loop, door_loop):
#     card_public_key = transform(7, card_loop)
#     door_public_key = transform(7, door_loop)
#     card_encryption_key = transform(door_public_key, card_loop)
#     door_encryption_key = transform(card_public_key, door_loop)
#     assert(card_encryption_key == door_encryption_key)
#

ans = 1
card_loop = 0
while ans != cpk:
    ans = (ans * 7) % base
    card_loop += 1
print('card_loop', card_loop)
print('encryption_key', transform(dpk, card_loop))
