# backend
old-be:
	cd backend && mvn spring-boot:run

be: docker-cleanup docker-network
	cd backend && mvn compile jib:dockerBuild -Dimage=covid-prediction-api
	docker run -it --network covid -p 8080:8080 --name covid-prediction-api covid-prediction-api

# frontend
install:
	cd frontend && npm i

fe: install
	cd frontend && npm run dev

docker: docker-cleanup docker-network docker-fe docker-be

docker-network:
	docker network create --driver bridge covid

docker-fe:
	cd frontend && docker build -t covid-prediction-ui .
	docker run -d --network covid -p 3000:3000 --name covid-prediction-ui covid-prediction-ui

docker-be:
	cd backend && mvn compile jib:dockerBuild -Dimage=covid-prediction-api
	docker run -d --network covid -p 8080:8080 --name covid-prediction-api covid-prediction-api

docker-cleanup:
	docker rm -f covid-prediction-ui covid-prediction-api || true
	docker network rm covid || true

deploy-vercel:
	cd frontend && npx vercel
