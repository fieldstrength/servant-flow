const api = require('./api')
const axios = require('axios')

const client = axios.create({
    baseURL: "http://localhost:8284",
    headers: {
        'Content-type': 'Application/json'},
    crossdomain: true})

//
// capture api
//
test('getCaptureByInt', () => api.getCaptureByInt(client, 66)
    .then(response => {
        expect(response.status).toBe(200)
        expect(response.data).toBe(66)
    }))

test('getCaptureByText', () => api.getCaptureByText(client, "howdy!")
    .then(response => {
        expect(response.status).toBe(200)
        expect(response.data).toBe("howdy!")
    }))

test('getCaptureByTrans', () => api.getCaptureByText(client, "ToUpper")
    .then(response => {
        expect(response.status).toBe(200)
        expect(response.data).toBe("ToUpper")
    }))

//
// query param api
//
test('getQueryparamInt', () => api.getQueryparamInt(client, {value: 66})
    .then(response => {
        expect(response.status).toBe(200)
        expect(response.data).toBe(66)
    }))

test('getQueryparamText', () => api.getQueryparamText(client, {value: "howdy!"})
    .then(response => {
        expect(response.status).toBe(200)
        expect(response.data).toBe("howdy!")
    }))

test('getQueryparamTrans', () => api.getQueryparamTrans(client, {value: "ToUpper"})
    .then(response => {
        expect(response.status).toBe(200)
        expect(response.data).toBe("ToUpper")
    }))


//
// reqbody api
//
test('postReqbodyByInt', () => api.postReqbodyInt(client, 66)
    .then(response => {
        expect(response.status).toBe(200)
        expect(response.data).toBe(66)
    }))

test('postReqbodyByText', () => api.postReqbodyText(client, "howdy!")
    .then(response => {
        expect(response.status).toBe(200)
        expect(response.data).toBe("howdy!")
    }))

test('postReqbodyByTrans', () => api.postReqbodyTrans(client, "ToUpper")
    .then(response => {
        expect(response.status).toBe(200)
        expect(response.data).toBe("ToUpper")
    }))

