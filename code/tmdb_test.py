import tmdbsimple as tmdb
import requests
import shutil


file1 = open("../data/celebrity_names.txt", "r+")

lines = file1.readlines()

count = 0 
while count < len(lines):
	lines[count] = lines[count].strip('\n')
	count += 1

print(lines)

tmdb.API_KEY = "f01f75eac6fbbd2fc5c105a021cb502e"

search = tmdb.Search()

for name in lines:
	response = search.person(query = name)
	base_image_url = "https://image.tmdb.org/t/p/w300_and_h450_bestv2"
	profile_path_url = response['results'][0]['profile_path']
	image_url = base_image_url + profile_path_url
	filename = "../data/" + name + ".jpg"
	r = requests.get(image_url, stream = True)

	if r.status_code == 200:
		r.raw.decode_content = True

		with open(filename, 'wb') as f:
			shutil.copyfileobj(r.raw, f)

		print('Image successfully downloaded: ', filename)
	else:
		print("Image couldnt be retreived")

# name = "Harrison Ford"

# response = search.person(query = name)

# #for s in search.results:
# #	print(s)

# print(response['results'][0]['profile_path'])

# #person = tmdb.People(search.results)
# #response = person.info()
# #print(response['profile_path'])

# base_image_url = "https://image.tmdb.org/t/p/w300_and_h450_bestv2"
# profile_path_url = response['results'][0]['profile_path']

# image_url = base_image_url + profile_path_url
# print(image_url)

# filename = name + ".jpg" #image_url.split("/")[-1]
# print(filename)

# r = requests.get(image_url, stream = True)

# if r.status_code == 200:
# 	r.raw.decode_content = True

# 	with open(filename, 'wb') as f:
# 		shutil.copyfileobj(r.raw, f)

# 	print('Image successfully downloaded: ', filename)
# else:
# 	print("Image couldnt be retreived")