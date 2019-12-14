class Family:

	def __init__(self, idx):
		self._childs = []
		self._id = idx
		self._husb_id = None
		self._wife_id = None
	
	def add_husband(self, idx):
		self._husb_id = idx
	
	def add_wife(self, idx):
		self._wife_id = idx

	def add_child(self, idx):
		self._childs.append(idx)
	
	@property
	def childs(self):
		return self._childs
	
	@property
	def husb_id(self):
		return self._husb_id
	
	@property
	def wife_id(self):
		return self._wife_id


class Person:

	def __init__(self, idx, name, surname, sex):
		self._id = idx
		self._name = name
		self._surname = surname
		self._sex = sex

	def get_full_name(self):
		return self._surname + ' ' + self._name

	def __str__(self):
		return self._name

	@property
	def sex(self):
		return self._sex


def parse(file_name):
	persons = {}
	families = {}
	person_id = None
	person_name = None
	person_surname = None
	person_sex = None
	fam_idx = None
	with  open(file_name, 'r') as f:
		for line in f:
			if 'INDI' in line:
				person_id = line.split()[1]
			elif 'NAME' in line:
				arr = line.split()
				person_name = arr[2]
			elif 'SURN' in line:
				person_surname = line.split()[2]
			elif 'SEX' in line:
				person_sex = line.split()[2]
				persons[person_id] = (Person(
					person_id,
					person_name,
					person_surname,
					person_sex
					))
			elif 'FAM\n' in line:
				families[line.split()[1]] = Family(line.split()[1])
				fam_idx = line.split()[1]
			elif 'HUSB' in line:
				families[fam_idx].add_husband(line.split()[2])
			elif 'WIFE' in line:
				families[fam_idx].add_wife(line.split()[2])
			elif 'CHIL' in line:
				families[fam_idx].add_child(line.split()[2])
	return persons, families


                

def get_predicats(families, persons):
	for key in families:
		for child in families[key].childs:
				print('parents(\'{}\', \'{}\', \'{}\')'.format(
				persons[child].get_full_name(),
				persons[families[key].husb_id].get_full_name(),
				persons[families[key].wife_id].get_full_name()))


def main():
	persons, families = parse('tree.txt')
	get_predicats(families, persons)


if __name__ == '__main__':
	main()
